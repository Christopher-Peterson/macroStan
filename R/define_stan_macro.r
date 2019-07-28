#' define_stan_macro: create a function that makes stan macros convienient
#' @param .args alist of argument names that must be subbed
#' @param coef name of the "end-product" created by the macro, to be used elsewhere in the stan code
#' @param functions functions block
#' @param data data block
#' @param td1 declarations in transformed data block
#' @param td2 after declarations transformed data block
#' @param parms parameters block
#' @param tp1 declarations in transformed parameters block
#' @param tp2 after declarations in transformed parameters block
#' @param model_decl local declarations in model block
#' @param prior prior distributions in model block
#' @param likelihood likelihood statements in model block
#' @param gq1 declarations in generated quantities block
#' @param gq2 after declarations in generated quantities block
#' @param ... named character vectors defining extra sections of the macro; these will be used with `glue`
#' @param .glue_control named list of control args for glue
#' @return a function that with arguments .section, ..., and whatever is in .args
#' @export
define_stan_macro = function(.args = alist(), coef = "", functions = "", data = "",
                             td1 = "", td2 = "", parms = "",tp1 = "",
                             tp2 = "", model_decl = "", prior = "", likelihood = "",
                             gq1 = "", gq2 = "", ...,
                             .glue_control = list(.open = "{{", .close = "}}")) {
  sections = list(functions = functions, coef = coef, data = data,
                  transformed_data_1=td1, td1 = td1,
                  transformed_data_2=td2, td2 = td2,
                  parameters = parms, parms = parms,
                  transformed_parameters_1 = tp1, tp1 = tp1,
                  transformed_parameters_2 = tp2, tp2 = tp2,
                  model_decl = model_decl, prior = prior, likelihood = likelihood, like = likelihood,
                  generated_quantities_1 = gq1, gq1 = gq1,
                  generated_quantities_2 = gq2, gq2 = gq2, ...)
  sec_names = force(c(".all", force(names(sections))))
  .args_full = c(.args, alist(...=, .section = sec_names))

  out_fun = function(To_be_replaced) {
    .section = match.arg(.section)
    arg_list = c(as.list(as.environment(-1L)), list(...))
    arg_list = arg_list[! names(arg_list) %in% c(".section", ".quote_args")]

    if(.section==".all") {
      out = as.list(vapply(sections, glue_args, "output is text",
                   args = arg_list, control = .glue_control))
      names(out) = names(sections)
    } else {
      sect = sections[[.section]]
      out = as.character(glue_args(sect, arg_list, .glue_control))
    }
    out
  }
  formals(out_fun) = .args_full
  class(out_fun) = c("stan_macro", "function")
  out_fun
}
#' returns logical
is_stan_macro = function(x) {
  "stan_macro" %in% class(x) && rlang::is_function(x)
}

# takes a function, wraps it in another function that quotes its args and converts them to characters
wrap_quote = function(.f) {
  function(...) {
  # browser()
    char_args = lapply(rlang::enexprs(...), rlang::expr_deparse)
    do.call(.f, char_args)
  }
}

# when called on a list of arguments, it makes sure any macros will automatically quote arguments
quote_macros = function(arg_list) {
  arg_names = names(arg_list)
  is_macro = vapply(arg_list, is_stan_macro, TRUE)
  arg_list[is_macro] = lapply(arg_list[is_macro], wrap_quote)
  setNames(arg_list, arg_names)
}

#' alternate paramterization for `glue_data`, easier to lapply with
#' @param what scalar character string to `glue_data`
#' @param args a list of data for `glue_data`
#' @param control a named list of control arguments for `glue_data`
#' @param ... additional data to be combined with `args`
#' @param .quote logical, indicating whether `stan_macros` in `what` should quote their arguments
#' @export
glue_args = function(what, args = list(),
                     control = list(.open = "{{", .close = "}}"), ...,
                     .quote = FALSE) {
  # browser()
  .x = c(args, list(...))
  if(isTRUE(.quote)) {
    .x = quote_macros(.x)
  }
  the_args = c(list(.x = .x, what), control)
  do.call(glue_data, the_args)
}



#' @export
summary.stan_macro = function(x, ...) {
  environment(x)$sections
}
#' @export
print.stan_macro = function(x, ...) {
  cat("Stan Macro:\n")
  print(summary(x))
}

# Look into @exportClass and @exportMethod
