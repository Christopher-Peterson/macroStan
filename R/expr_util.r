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

# takes an unevaluated function call, and converts all args to character vectors
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

#Document these:

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
# Vectorized version of assignment_to_arg
assignments_to_arg = function(.l) {
  # all elements of
  to_change = vapply(.l, is.language, TRUE)
  out = .l
  out[to_change] = lapply(.l[to_change],assignment_to_arg)
  out[!to_change] = lapply(.l[!to_change], list)
  do.call(c, out)
}

#' parse a text assignment, quoting what needs to be quoted
#' @param x text of an assignment
#' @param .def_side if there's no assignment, what should it be treated as?
parse_assignment = function(x, .def_side) {
  # browser()
  if(!(grepl("=", x, fixed = TRUE) ||
       grepl("<-", x, fixed = TRUE))) {
    if(.def_side == "rhs"){
      out = glue::glue('"{x}"')
    } else out = x
  } else {
    pos = vapply(c("=", "<-"), regexpr,
               text = x, fixed = TRUE, 1L)
    if(pos[2] > pos[1]) extra = 2 else extra = 1
    pos = max(pos)
    rh = substr(x, 1, pos-1)
    lh = trimws(substring(x, pos+extra))
    out = glue::glue('{rh} = "{lh}"')
  }
  # browser()
  rlang::parse_expr(out)
}

#' Some stan code won't parse to R.  The only time that it's needed
#' is when figuring out assignments
#' This tries to parse to R; if it fails, it wraps the lhs in quotes
#' and parses that
#' @param x_lst text of an assignment to be parsed
#' @param .def_side if there's no assignment, what should it be treated as?
parse_assignments = function(x_lst, .def_side = c("lhs", "rhs")){
  .def_side = match.arg(.def_side)
  lapply(x_lst, parse_assignment, .def_side = .def_side)
}



