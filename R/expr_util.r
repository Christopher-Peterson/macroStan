# Utilities for expression manipulation

#' Checks to see if an expression is an assignment;
#' @param x a language object to examine
#' @return logical
is_assignment = function(x) {
  if(!is.language(x)) stop("x must be a language object (try using rlang::parse_expr())")
  if(length(x) == 1) return(FALSE)
  op = x[[1]]
  identical(op, rlang::expr(`=`)) | identical(op, rlang::expr(`<-`))
}

# Returns the lhs for an expression if it's an assignment; else, assume its a name
assignment_lhs = function(arg_expr) {
  if(is_assignment(arg_expr)) arg_expr = arg_expr[[2]]
  arg_expr
}
assignment_rhs = function(arg_expr) {
  if(is_assignment(arg_expr)) {
    return(arg_expr[[3]])
  } else {
    return(rlang::missing_arg())
  }
}

#' takes an unevaluated function call, and converts all args to character vectors
args_as_char = function(.call) {
  if(is_assignment(.call)) {
    out = assignment_rhs(.call)
  } else {
    out = .call
  }
  # We are assuming that unnamed arguments are values, not names
  arg_seq = seq(2, length(out))
  out[arg_seq] = lapply(out[arg_seq],
                        function(.x) rlang::parse_expr(glue::glue("\"{.x}\"")))
  if(is_assignment(.call)) {
    .call[[3]] = out
    out = .call
  }
  out
}

# takes language x and returns x = NULL
null_expr = function(x) {
  out = rlang::parse_expr("x = NULL")
  out[[2]] = x
  out
}
make_missing_arg = function(x) {
  if(is.language(x)) x = as.character(x)
  setNames(as.list(rlang::parse_expr("alist(a =)"))[2], x)
}
assignment_to_arg = function(x) {
  if(!is_assignment(x)) stop(as.character(x), " is not an assignment")
  val = as.character(x[[3]])
  nm = as.character(x[[2]])
  setNames(list(tmp = val), nm)
}
