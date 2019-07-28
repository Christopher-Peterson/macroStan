#' define_stan_macro: create a function that makes stan macros convienient
#' @param .args alist of argument names that must be subbed
#' @param ... named character vectors defining a section of the macro; these will be used with `glue`
#' @param .glue_control named list of control args for glue
#' @return a function that with arguments .section, ..., and whatever is in .args
#' @export
define_stan_macro = function(.args = alist(), ..., .glue_control = list()) {
  sections = list(...)
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

#' alternate paramterization for glue_data, easier to lapply with
# glue_args = function(what, args = list(), control = list(), ...) {
#   the_args = c(list(.x = c(args, list(...)), what), control)
#   do.call(glue_data, the_args)
# }

# takes a function, wraps it in another function that quotes its args and converts them to characters
wrap_quote = function(.f) {
  function(...) {
  # browser()
    char_args = lapply(rlang::enexprs(...), rlang::expr_deparse)
    do.call(.f, char_args)
  }
}
is_stan_macro = function(x) {
  "stan_macro" %in% class(x) && rlang::is_function(x)
}
# when called on a list of arguments, it makes sure any macros will automatically quote arguments
quote_macros = function(arg_list) {
  arg_names = names(arg_list)
  is_macro = vapply(arg_list, is_stan_macro, TRUE)
  arg_list[is_macro] = lapply(arg_list[is_macro], wrap_quote)
  setNames(arg_list, arg_names)
}

#' alternate paramterization for glue_data, easier to lapply with
glue_args = function(what, args = list(), control = list(), ...,
                     .quote = FALSE) {
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
