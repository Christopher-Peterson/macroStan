
#' parse_stan_macros: read in a stan file and replace macros
#' @param input either a file name or a the text of a stan file
#' @param out_file output file (can be NA)
#' @param ... named arguments for string interpolation
#' @param .macro_symbol character identifying the beginning of a macro
#' @param .glue_control named list of arguments to glue, includeing .open and .close
#' @param .macro_list a list of arguments to be used for glue's string interpolation
#' @param .quote_args logical; if true, macro calls in the stan file will have arguments quoted instead of evaluated
#' @return the parsed stan code if `out_file = NA`; otherwise `out_file`
#' @export
parse_stan_macros = function(input, out_file = NA, ...,
                             .macro_symbol = "$",
                             .macro_list = list(),
                             .glue_control = list(), .quote_args = TRUE) {
  # Determine glue open and close
  .glue_control = set_open_close(.glue_control, .macro_symbol)
  # read in file, replace line macros with wrapped macros
  input_code = get_input_code(input)
  wrapped_code = wrap_line_macros(input_code, .macro_symbol, .glue_control)
  # re-collapse the code into a length 1
  col_code = glue::glue_collapse(wrapped_code, sep = "\n")
  # browser()
  out_code = glue_args(col_code, args = .macro_list,
                       control = .glue_control, ..., .quote = .quote_args)

  # Write out file, if possible
  if(!is.na(out_file)) {
    writeLines(out_code, out_file)
    out = out_file
  } else {
    out = out_code
  }
  # Check to see if the final code is valid
  rstan::stanc(model_code = out_code)
  out
}

# Delimiters should have the macro symbol appended before or after them
fix_delimiter = function(x, default = "{{") {
  x = ifelse(is.null(x), default, x)
  x
}
set_open_close = function(control=list(), .macro_symbol = "$") {
  control$.open = fix_delimiter(control$.open)
  control$.close = fix_delimiter(control$.close, default = "}}")
  control
}

get_input_code = function(input) {
  if(length(input) == 1 &&
     file.exists(input) &&
     tolower(tolower(tools::file_ext(input))) == "stan") {
    input_code = readLines(input)
  } else {
    # Each line should be a different element of the vector
    input_code = unlist(strsplit(input, split = "\n", fixed = TRUE))
  }
  input_code
}

# lines that start with the symbol are wrapped in the delimiters
wrap_line_macros = function(code, .macro_symbol = "$",
             .glue_control = set_open_close(list(), .macro_symbol)) {
  .o = .glue_control$.open
  .c = .glue_control$.close
  tws_code = trimws(code)
  line_macros = startsWith(tws_code, .macro_symbol) &
    (!startsWith(tws_code, .o))
  if(any(line_macros)) {
    w_code = tws_code[line_macros]
    code[line_macros]  = as.character(
      glue("{.o}{x}{.c}", x = substr(w_code, nchar(.macro_symbol) + 1, nchar(w_code))))
  }
  code
}
