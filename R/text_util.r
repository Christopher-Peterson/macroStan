# Some utility functions for text manipulation

# collapses lines together
collapse_lines = function(x) {
  as.character(glue::glue_collapse(x, sep = "\n"))
}
# replaces template with equivalent length whitespace
null_string = function(template, char_out = " ") {
  as.character(glue::glue_collapse(rep(char_out, nchar(template))))
}

# this is used to find matched delimiters
# it returns the position of each right and left delim,
# in order.
get_delim_tbl = function(x, .left = "{", .right = "}") {
  position_table = rbind(
    data.frame(pos = unlist(gregexpr(.left, x, fixed = TRUE )),
               char = .left, val = 1, stringsAsFactors = FALSE),
    data.frame(pos = unlist(gregexpr(.right, x, fixed = TRUE )) + (nchar(.right)-1),
               char = .right, val = -1, stringsAsFactors = FALSE)
  )

  position_table = subset(position_table, pos >0)
  sorted = position_table[order(position_table$pos), ]
  # drop rows before first .left
  if(nrow(sorted) > 0) {
    first_start = first(sorted$val==1)
    sorted = subset(sorted, seq_len(nrow(sorted)) >= first_start)
  }
  # cs is the cumulative sum of left(1) and right(-1) delims; when cs=0, the outer condition has been met
  sorted$cs = cumsum(sorted$val)
  sorted
}

# returns what's inside the first full pair of delimeters
get_delim_contents = function(x, .left = "{", .right = "}") {
  x = collapse_lines(x)
  sorted = get_delim_tbl(x, .left, .right)
  last = which(sorted$cs == 0)[1]
  substr(x, sorted$pos[1] + nchar(.left),
         sorted$pos[last] - nchar(.right))
}

get_all_delim_contents = function(x, .left, .right) {
  x = collapse_lines(x)
  pos_tbl = get_delim_tbl(x, .left, .right)
  ends = filter_rows(pos_tbl, cs == 0)$pos
  starts = filter_rows(pos_tbl, cs == 1 & pos_tbl$char == .left)$pos
  vapply(seq_along(starts), function(i) {
    substr(x, starts[i] + nchar(.left), ends[i] - nchar(.right))
  }, "chr")
}

# returns x, but with the delim and its contents removed
blank_delim_contents = function(x, .left = "{", .right = "}") {
  to_replace = paste0(.left,
                get_all_delim_contents(x, .left, .right), .right)
  nullify_string = function(x, pattern) {
    gsub(pattern, null_string(pattern), x, fixed = TRUE)
  }
  Reduce(nullify_string, to_replace, x)
}
# a version of the function for Reduce()
# delims should be a length-2 character vector c(.left, .right)
blank_delims_red = function(x, .delims) {
  blank_delim_contents(x, .delims[1], .delims[2])
}

# take a character vector of an argument list
# return a vector of unparsed arguments
separate_commas = function(x, delim_list =
             list(c("{","}"), c("(",")"), c("[","]"))){
  # remove everything in parentheses, brackets, etc
  # so that all remaining commas are top-level
  x_blank = Reduce(blank_delims_red, delim_list, x)
  # Where are the commas located?
  comma_pos = unlist(gregexpr(",", x_blank, fixed = TRUE))
  if(comma_pos[1] == -1) return(list(x))
  trimws(unlist(mapply(function(from, to) substr(x, from, to),
         from = c(0, comma_pos + 1),
         to = c(comma_pos - 1, nchar(x)), SIMPLIFY = FALSE)))
    # x = "a, b = c(d,e), f = [a, b]', g = {r}, duh"
}


# this searches for left and right delimiters, but in a non-nested sense
# The next one it finds gets triggered
extract_delim_linear = function(x, .left, .right) {
  if(length(x)!=1) stop("x must be length 1")
  left_side = as.numeric(regexpr(.left, x, fixed = TRUE))
  out = list(out =  data.frame(.left = integer(0), .right = integer(0),
                               .sym = character(0), text = character(0)),
             x = x)
  if(left_side > 0) {
    # browser()
    right_side = unlist(gregexpr(.right, x, fixed = TRUE))
    right_side = right_side[right_side > left_side]
    if(length(right_side) > 0) {
      right_side = min(right_side) + nchar(.right) - 1
      out_text = substr(x, left_side, right_side)
      out = list(out = data.frame(.left = left_side, .right = right_side ,
                                  .sym = .left, text = out_text, stringsAsFactors = FALSE),
                 x = sub(out_text, null_string(out_text), x, fixed = TRUE))
    }
  }
  out
}

# This pulls out all text groups surrounded by the delims, from left to right
extract_all_delims_linear = function(x, .left, .right) {
  # browser()
  if(is.list(x) && all(names(x) == c("out", "x"))) {
    .last_out = list(x$out)
    x = x$x
  } else {
    x = collapse_lines(x)
    .last_out = list()
  }
  reduce_fun = function(.last, .count) {
    # browser()
    .next = extract_delim_linear(.last$x, .left, .right)
    out = list(out = c(.last$out, list(.next$out)),
               x = .next$x)
    out
  }
  # max number of steps for reduce
  counter = seq_along(unlist(gregexpr(.left, x, fixed = TRUE)))
  out = Reduce(reduce_fun, counter, init = list(out=.last_out, x=x))
  # browser()
  out$out = do.call(rbind, out$out)
  out
}

# This removes a number of delimiter-based sequences
# .lefts and .rights much match in length
# there are removed sequentially, by delimiter then from right to left
extract_sequence = function(x, .lefts = c("//", "/*"), .rights = c("\n", "*/")){
  if(length(.lefts) != length(.rights)) stop(".lefts and .rights must be the same length")
  reduce_fun = function(.last, .iter) {
    extract_all_delims_linear(.last, .lefts[.iter], .rights[.iter])
  }
  # browser()
  out = Reduce(reduce_fun, x = seq_along(.lefts), init = x)
  # if(is.null(out$out)) {
  #   out$out =
  # }
  out
}

# removes empty strings from a character vector
remove_empty_strings = function(.l) {
  empty = vapply(.l, function(.x) .x == "", TRUE)
  .l[!empty]
}

# TEST: THIS
insert_string = function(old_string, position, new_string, .sep = "\n") {
  paste(substr(old_string, 0, position-1),
        new_string,
        substr(old_string, position, nchar(old_string)),
        sep = .sep)
}

get_line_of_tag = function(x, tag, fixed = TRUE, ...){
  # Assumes x length 1
  splits = strsplit(x, "\n", fixed = TRUE)[[1]]
  which_line = first(grepl(tag, splits, fixed = fixed, ...))
  unlist(regexec(splits[which_line], x, fixed = fixed, ...)) - 1
}

#' alternate paramterization for `glue_data`, easier to lapply with
#' @param what scalar character string to `glue_data`
#' @param args a list of data for `glue_data`
#' @param control a named list of control arguments for `glue_data`
#' @param ... additional data to be combined with `args`
glue_args = function(what, args = list(),
                     control = list(.open = "{{", .close = "}}"), ...) {
  # browser()
  .x = c(args, list(...))
  the_args = c(list(.x = .x, what), control)
  do.call(glue_data, the_args)
}
