# Miscellaneous utility functions
# several of these are quick and dirty dplyr replacements
# they should not be used outside of inside these functions,
# because they haven't been nearly as thoroughly tested

#' return the index of the first element of x_lgl that is TRUE
#' returns empty vector if none apply
nth = function(x_lgl, n = 1) {
  if(sum(x_lgl) < n) return(integer(0))
  sort(which(x_lgl))[n]
}
first = function(x_lgl) nth(x_lgl, 1L)

#' sorts a data frame
#' @param .df a data frame
#' @param .expr what to sort by; can be in terms of columns of `.df`
#' @param .desc logical; if true, sorts by descending order (otherwise ascending)
sort_by = function(.df, .expr, .desc = FALSE) {
  the_vec = rlang::eval_tidy(rlang::enquo(.expr), data = .df)
  if(isTRUE(.desc)) the_vec = -the_vec
  .df[order(the_vec),]
}

# Basically dplyr::filter with one condition
#' @param .df a data frame
#' @param .cond a logical statement about .df
filter_rows = function(.df, .cond) {
  cond = rlang::eval_tidy(rlang::enquo(.cond), data = .df)
  if(!is.logical(cond)) stop(".cond must return a logical value")
  .df = .df[cond, ]
  .df
}
