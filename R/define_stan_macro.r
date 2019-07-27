#' define_stan_macro: create a function that makes stan macros convienient
#' @param .args alist of argument names that must be subbed
#' @param ... named character vectors defining a section of the macro; these will be used with `glue`
#' @param .glue_control named list of control args for glue
#' @return a function that with arguments .section, ..., and whatever is in .args
#' @export
define_stan_macro = function(.args = alist(), ..., .glue_control = list()) {
  sections = list(...)
  sec_names = force(c(".all", force(names(sections))))
  .args = c(.args, alist(...=, .section = sec_names))

  out_fun = function(To_be_replaced) {
    .section = match.arg(.section)
    # create a name list of arguments
    arg_list = c(as.list(as.environment(-1L)), list(...))
    arg_list = arg_list[names(arg_list) != ".section"]

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
  formals(out_fun) = .args
  class(out_fun) = c("stan_macro", "function")
  out_fun
}

#' alternate paramterization for glue_data, easier to lapply with
glue_args = function(what, args, control = list(), ...) {
  the_args = c(list(.x = c(args, list(...)), what), control)
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
