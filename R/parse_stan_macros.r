
#' parse_stan_macros: read in a stan file and replace macros
#' @param input either a file name or a the text of a stan file
#' @param out_file output file (can be NA)
#' @param ... named arguments for string interpolation
#' @param .macro_symbol character identifying the beginning of a macro
#' @param .glue_control named list of arguments to glue, includeing .open and .close
#' @param .macro_list a list of arguments to be used for glue's string interpolation
#' @return the parsed stan code (invisibly if `out_file = NA`)
#' @export
parse_stan_macros = function(input, out_file = NA, ...,
                             .macro_symbol = "$",
                             .macro_list = list(),
                             .glue_control = list()) {
  # Determine glue open and close
  .glue_control = set_open_close(.glue_control, .macro_symbol)
  # read in file, replace line macros with wrapped macros
  input_code = get_input_code(input)
  wrapped_code = wrap_line_macros(input_code, .macro_symbol, .glue_control)
  # re-collapse the code into a length 1
  col_code = glue::glue_collapse(wrapped_code, sep = "\n")
  out_code = glue_args(col_code, args = .macro_list,
                       control = .glue_control, ...)

  # Write out file, if possible
  if(!is.na(out_file)) {
    writeLines(out_code, out_file)
    ret = invisible
  } else ret = return
  # Check to see if the final code is valid
  tmp = rstan::stanc(model_code = out_code)
  ret(out_code)
}

# Delimiters should have the macro symbol appended before or after them
fix_delimiter = function(x, symbol, default = "{") {
  x = ifelse(is.null(x), default, x)
  if(!startsWith(x, symbol)) {
    x = ifelse(default == "{", paste0(symbol, x), paste0(x, symbol))
  }
  x
}
set_open_close = function(control=list(), .macro_symbol = "$") {
  control$.open = fix_delimiter(control$.open, .macro_symbol)
  control$.close = fix_delimiter(control$.close, .macro_symbol, default = "}")
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
