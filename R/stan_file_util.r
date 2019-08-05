# functions involving general parsing of stan files

list_block_names = function(.macro = FALSE) {
  out = c("functions", "data", "transformed data",
          "parameters", "transformed parameters",
          "model", "generated quantities")
  if(isTRUE(.macro)) out = c("macro args", "use macros", out)
  out
}

# Why did I use this again?
block_size = function(x) {
  if(is.null(x)) x = ""
  nchar(x) + 1
}

# removes commented parts of a stan block, replacing with the same amount of white space
remove_comments = function(x) {
  # append a final "\n" to x before using, so that final line comments will terminate
  extract_sequence(x, .lefts = c("//", "/*" ),
                   .rights = c("\n", "*/"))$x
}

# this detects any internal blocks (beginning with "{")
# and removes the rest of the vector.  I could write a more
# sophisticated version of this that actually detects and
# removes just the block, but that isn't necessary for figuring out
# where the declarations end.
remove_internal_blocks = function(x, delim = "{|") {
  # browser()
  # Removes left delim, to avoid bracket confusion
  x_tmp = gsub(delim, null_string(delim), x, fixed = TRUE)
  # browser()
  bracket_locs = unlist(gregexpr("{", x_tmp, fixed = TRUE))
  if(bracket_locs[1] == -1) {
    out = x
  } else {
    out = substr(x, 1, min(bracket_locs))
  }
  out
}


# parse a stan file and pull out the corresponding block
# note that two extra block names are here
find_block = function(x, block = list_block_names(.macro = TRUE), ...) {
  block = match.arg(block)
  x = as.character(x)
  # separate lines into elements
  x = unlist(strsplit(x, "\n", fixed = TRUE))
  tws_x = trimws(x)
  # find the line starting with the block label and ends with a brace
  start_pos = startsWith(tws_x, prefix = block) & endsWith(tws_x, "{")
  if(!any(start_pos)) return("")
  get_delim_contents(x[seq(which(start_pos)[1], length(x), by = 1)])
}
# determine where declarations inside a block end;
# returns the character position immediately after declarations
post_declaration_position = function(x, .left = "{|") {
  if(is.null(x) || x == "") return (1)
  # Returns an iteger of the first character position after declarations end
  base_data_types = c("int", "real",
                      "vector","row_vector", "matrix",
                      "simplex", "ordered", "unit_vector",
                      "positive_ordered",
                      "cov_matrix", "corr_matrix",
                      "cholesky_factor_cov",
                      "cholesky_factor_corr")
  # Technically, this includes invalid types int< and real<
  # but it shouldn't matter; the resulting file will fail in that case
  data_types = c(glue::glue("{base_data_types} "),
                 glue::glue("{base_data_types}["),
                 glue::glue("{base_data_types}<"))
  # browser()
  # browser()
  x = remove_internal_blocks(remove_comments(x), delim = .left)
  declarations = extract_sequence(x, data_types,
                                  rep(";",length(data_types)))$out
  # if any rows, get max value of .right, add 1; this is the second group
  if(nrow(declarations) == 0) {
    out = 1
  } else {
    out = max(declarations$.right) + 1
  }
  out
}

# splits a block into pre and post declarations
separate_declarations = function(x) {
  dec_pos = post_declaration_position(x)
  # browser()
  out = list(declarations = substr(x, 1, dec_pos-1),
             post = substr(x, dec_pos, nchar(x)))
  out
}

#' transforms a stan file into list with named elements corresponding to the program block
#' @param x stan code
get_blocks = function(x) {
  block_list = list_block_names(TRUE)
  separated_blocks = remove_empty_strings(
    setNames(
      lapply(block_list, function(.block)
        find_block(x, block = .block)), block_list))
  # browser()
  dec_blocks = c("transformed data", "transformed parameters", "model", "generated quantities")
  dec_blocks = dec_blocks[dec_blocks %in% names(separated_blocks)]
  separated_blocks[dec_blocks] =
    lapply(separated_blocks[dec_blocks], separate_declarations)
  separated_blocks
}
