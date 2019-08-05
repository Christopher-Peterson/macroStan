# parse stan macros

#' inserts a block of macro(s) into a block of scaffold
#'@param .scaffold one stan block to insert into
#'@param .macros character vector of parsed, merged stan macros for the block
#'@param pos where to insert.  If NA, automatically inserts after declarations (if in an appropriate block) or at the end (if not)
insert_macro_section = function(.scaffold, .macros, pos = NA) {
  if(is.na(pos))  pos = post_declaration_position(.scaffold)
  insert_string(.scaffold, pos, .macros, .sep = "")
}

#' This should be a safe way of getting a macro's definition
#' @param macro_name character vector
#' @param macro_list list of `stan_macro` objects
macros_exist = function(macro_names, macro_list) {
  if(!all(macro_names %in% names(macro_list))) {
    missing_nms = macro_names[!macro_names %in% names(macro_list)]
    stop(macro_name, " is not in list of available macros")
  }
  invisible(TRUE)
}

#' determine if a macro has a value argument
#' @param macro_fun object of class `stan_macro`
has_value = function(macro_fun) {
  args = names(formals(macro_fun))
  "value" %in% args
}

#' converts expression from x = foo(bar = 1) to foo(bar = 1, value = x)
#' ignores non-assignment expressions
#' @param .expr language object
parse_value = function(.expr) {
  if(!is_assignment(.expr)) {
    .out = parse_macro_function(.expr)
  } else {
  # browser()
  .out = parse_macro_function(.expr[[3]])
  .out$value = parse_assignment(.expr[[2]], "rhs")
  }
  .out
}

# First, put everything in parantheses in quotes
parse_macro_function = function(x) {
  in_paren = get_delim_contents(x, "(", ")")
  # Now, we have a list of args
  args = assignments_to_arg(parse_assignments(
    separate_commas(in_paren), "rhs"))
  base_call = rlang::parse_expr(blank_delim_contents(x, "(", ")"))
  as.call(c(base_call, args))
}
#' extracts the macro calls from the use macros block
#' @param blocks a macro file parsed with `get_blocks`
#' @return a list of language objects, one per argument
get_macro_calls = function(blocks) {
  out = parse_macro_block(blocks, "rhs", "use macros")
  lapply(out, parse_value)
}

#' identify which macros are being used and parse them into text
#' @param scaffold_blocks `get_blocks()` of the scaffold
#' @param macro_list list of available macros
parse_macro_calls =
  function(scaffold_blocks, macro_list) {
    macro_calls = get_macro_calls(scaffold_blocks)
    macro_names = vapply(macro_calls, function(.x) as.character(.x[[1]]), "chr")
    macros_exist(macro_names, macro_list)
    rlang::eval_tidy(lapply(macro_calls, args_as_char),
                     data = macro_list)
  }

#' parse the used macros into ordered text,ready for insertion
#' @param blocks `get_blocks()` output for the scaffold
#' @param macro_list list of available macros
#' @return a named list of blocks
order_stan_macros = function(blocks, macro_list = list()) {
  # browser()
  if(!"use macros" %in% names(blocks)) stop("No macros defined.")
  macro_code_list =
    lapply(parse_macro_calls(blocks, macro_list),
           rlang::eval_tidy,data = macro_list)
  # remove linebreaks, split by semicolon
  block_names = list_block_names(FALSE)
  # browser()
  block_code =  setNames(
    lapply(block_names, function(block) {
      lapply(macro_code_list, getElement, name = block)
    }), block_names)
  one_part = c("functions", "data", "parameters")
  two_part = block_names[!block_names %in% one_part]
  # check ot make sure names are still there!
  # re-order the two-part code blocks
  block_code[two_part] = lapply(block_code[two_part],
    function(block) c(lapply(block, getElement, name ="declarations"),
                    lapply(block, getElement, name ="post")))
  unlist(block_code$`generated quantities`)
  out = lapply(block_code, function(block) {
    paste(unlist(block), collapse = "\n")})
  out[out != ""]
  # check names
}

#' @param scaffold `stan_blocks` of scaffold
#' @param block_names names of blocks to insert
block_insert_functions = function(scaffold, block_names) {
  # browser()
  insert_function_list = list(
    one = function(.scaffold, .macro) {
      paste(.scaffold, .macro, sep = "\n")
    }, two = function(.scaffold_l, .macro) {
      paste(.scaffold_l$declarations,
            .macro, .scaffold_l$post, sep = "\n")
    })

  key = list(
    functions = 1, data = 1, `transformed data` = 2,
    parameters = 1, `transformed parameters` = 2,
    model = 2, `generated quantities` = 2)

  insert_funs = lapply(block_names,
          function(.nm)  insert_function_list[[key[[.nm]]]])
  setNames(insert_funs, block_names)
}

#' insert macros into scaffold
#' @param scaffold_list a `stan_blocks` list to be inserted into
#' @param macro_blocks a `stan_blocks` list of parsed macros to insert
insert_macros = function(scaffold_list, macro_blocks) {

  block_names = list_block_names(FALSE)
  # browser()
  block_names = block_names[
    block_names %in% c(names(macro_blocks), names(scaffold_list))]
  names(block_names) = block_names ## this is for the lapply later
  insert_functions = block_insert_functions(scaffold_list, block_names)
  # insert_functions = block_insert_functions(scaffold_list, macro_names)

  lapply(block_names, function(.nm) {
    insert_functions[[.nm]](scaffold_list[[.nm]], macro_blocks[[.nm]])
  })
}

#' @param block_list a `stan_blocks` object
#' @return text of a stan file
blocks_to_stan_string = function(block_list) {
  # browser()
  nms = names(block_list)
  collapse_lines(vapply(nms, function(.nm) {
    glue::glue("{|.nm|} { \n {|trimws(block_list[[.nm]]) |} \n } ",
               .open = "{|", .close = "|}")
  }, "char"))
}

#' checks to make sure all provided macros (and files) are of the valid class; returns combined list
#' @param macro_files named list of macro file names
#' @param macro_list named list of macro definitions
#' @param ... additional named macro definitions
verify_macro_list = function(macro_files, macro_list, ...) {
  all_macros = c(macro_list, get_macro_file_list(macro_files), ...)
  macro_check = vapply(all_macros, is_stan_macro, TRUE)
  if(is.null(names(all_macros)) || "" %in% names(all_macros)) {
    stop("All macros must be named")
  }
  if(length(all_macros) != length(unique(names(all_macros)))) {
    stop("All macros must have distinct names")
  }
  if(!all(macro_check)) {
    stop("Macro defintions not valid: ", names(all_macros)[!macro_check])
  }
  all_macros
}

# Determines whether the input is a file or charcter vector of stan code
#' @param input from parse_stan_macros
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

#' Applies macros to a stan file
#' @param input stan code or file name
#' @param output file name to output
#' @param macro_files named list of macro file names
#' @param macro_list named list of macro definitions
#' @param ... additional named macro definitions
#' @param .validate_output check to make sure output stan file is valid
#' @return the parsed stan code if `out_file = NA`; otherwise `out_file`
#' @export
parse_stan_macros = function(input, output_file,
                             macro_list = list(),
                             macro_files = list(), ...,
                             .validate_output = TRUE) {
  all_macros = verify_macro_list(macro_files, macro_list, ...)
  scaffold_code = get_input_code(input)
  scaffold = get_blocks(scaffold_code)
  # browser()
  macros = order_stan_macros(scaffold, all_macros)
  out_code = blocks_to_stan_string(insert_macros(scaffold, macros))
  if(!is.na(output_file)) {
    writeLines(out_code, output_file)
    out = output_file
  } else {
    out = out_code
  }
  if(isTRUE(.validate_output)) rstan::stanc(model_code = out_code)
  out
}



