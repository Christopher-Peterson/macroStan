#' generic function used by get_macro_calls and get_macro_args
#' @param blocks a macro file parsed with `get_blocks`
#' @param .def_side for `parse_assignments`
#' @param block_name "use macros" or "macro args"
parse_macro_block = function(blocks, .def_side = c("rhs", "lhs"),
                             block_name = c("use macros", "macro args")){
  block_name = match.arg(block_name)
  err_msg = ifelse(block_name == "use macros", "No macros defined", "Not a macro")
  if(!block_name %in% names(blocks)) stop(err_msg)
  # remove linebreaks, split by semicolon
  no_comments = remove_comments(blocks[[block_name]])
  call_txt = remove_empty_strings(trimws(
    strsplit(no_comments, split = ";", fixed = TRUE)[[1]]))
  parse_assignments(call_txt, .def_side = .def_side)
}

#' #' extracts the arguments from a macro args block
#' #' @param blocks a macro file parsed with `get_blocks`
#' #' @return a list of language objects, one per argument
get_macro_args = function(blocks) {
  parse_macro_block(blocks, "lhs", "macro args")
}

#' @param .args list of language objects returned by `get_macro_args`
#' @return a character vector of argument names
get_arg_names = function(.args) {
  assigns = vapply(.args, is_assignment, TRUE)
  # get names from assignments
  .args[assigns] = lapply(.args[assigns],
     function(.x) .x[[2]])
  arg_lengths = vapply(.args, length, 1L)
  if(any(arg_lengths != 1)) {
    stop("invalid macro args specification:", .args[arg_lengths != 1])
  }
  vapply(.args, as.character, "char")
}

#' checks to make sure that only defined arguments are used for splicing in a macro
#' @param blocks a macro file parsed with `get_blocks`
#' @return `invisible(blocks)`, if valid; otherwise throws an error`
validate_macro_args =
  function(blocks, args, .left = "{|", .right = "|}") {
  # browser()
  arg_names = get_arg_names(args)
  u_blocks = unlist(blocks, use.names = FALSE)

  spliced_terms =
    trimws(get_all_delim_contents(u_blocks, .left, .right))

  if(!all(spliced_terms %in% arg_names)) {
    extras = spliced_terms[!spliced_terms %in% arg_names]
    stop("Undefined arguments used in macro: ", extras)
  }
  invisible(blocks)
}

#' @param .args list of language objects returned by `get_macro_args`
#' @return formals for a new function
parse_macro_formals = function(.args) {
  just_names = vapply(.args, length, 1L) == 1L
  out = vector("list", length(.args))
  out[just_names] = lapply(.args[just_names], make_missing_arg)
  out[!just_names] = lapply(.args[!just_names], assignment_to_arg)
  unlist(out, recursive = FALSE)
}

#' Read in a macro file
#' @param file file name to read in; ignored if `macro_code` is used
#' @param macro_code text representation of the macro;
#' @return a stan_macro object, which is a function that returns parsed macro code
#' @export
read_macro = function(file, macro_code = readLines(file), .glue_control =
                        list(.open = "{|", .close = "|}")) {
  macro_blocks = get_blocks(macro_code)
  # browser()
  args = get_macro_args(macro_blocks)
# Somewhere here, add the option to nest macros!
# Add an extra arg for previously defined macros
# At some point, write an algorithm to determine which macros
# depend on which other ones, and parse them in reverse order

  validate_macro_args(macro_blocks, args)
  formal_args = parse_macro_formals(args)

  # browser()
  # remove macro_args, use macro
  sections = macro_blocks[!names(macro_blocks) %in%
     c("macro args", "use macro")]
  rm(macro_code, macro_blocks)
  # browser()
  out = function(to_be_replaced){
    # browser()
    arg_list = as.list(as.environment(-1L))
    glue_it = function(.x) as.character(
      glue_args(.x, args = arg_list, control = .glue_control))
    # sizes = vapply(code_blocks, length, 1)
    sapply(sections, function(block) {
      if(length(block) > 1) {
        out = as.list(vapply(block, glue_it, "abcd"))
      } else {
        out = glue_it(block)
      }
      out
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  formals(out) = formal_args
  class(out) = c("stan_macro", "function")
  out
}

#' returns logical
is_stan_macro = function(x) {
  "stan_macro" %in% class(x) && rlang::is_function(x)
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

#' @param macro_files a list of file names, corresponding to macros
get_macro_file_list = function(macro_files) {
  # check names
  if("" %in% names(macro_files)|| is.null(names(macro_files)))
    stop("all elements of macro_files must be named")
  existing = vapply(macro_files, file.exists, TRUE)
  if(!all(existing)) {
    stop("Files do not exist: ", macro_files[!existing])
  }
  lapply(macro_files, read_macro)
}
