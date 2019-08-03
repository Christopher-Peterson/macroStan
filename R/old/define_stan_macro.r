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


