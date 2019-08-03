test_that("collapse_lines works", {
  expect_equal(collapse_lines(c("a", "b", "c", "d")), "a\nb\nc\nd")
})

test_txt = c("test {
  a {}, b{ c{, },
  another big test here is odd",",
  {yet again} today},
  what??? } { }")

test_that("get_delim_tbl can sort out some complex nesting", {
  dlt = get_delim_tbl(collapse_lines(test_txt))
  expect_equal(
    dlt$pos[min(which(dlt$cs == 0))],
    nchar(collapse_lines(test_txt)) - 4)
})
test_that("get_delims_outer works", {
  manual = sub("} { }","",
     sub("test {", "", collapse_lines(test_txt),
         fixed = TRUE), fixed = TRUE)
  auto = get_delim_contents(test_txt)
  expect_equal(auto, manual)
})
test_that("null_string works",{
  # don't use regular expressions or escapes in this
  test_str = "1234502-9823-098dfjl aasdfl  poiasdjf;lk"
  null_str = null_string(test_str)
  num_spaces = attr(regexec(pattern = "[ ]*", text = null_str)[[1]], "match.length")

  expect_equal(nchar(test_str),num_spaces)
  expect_equal(nchar(null_str), nchar(test_str))
})

test_that("extract_delim_linear doesn't work with length(x) > 1",{
  expect_error(extract_delim_linear(c("a", "b"), "l", "r"))
})
test_that("extract_delim_linear doesn't work stuff that can't be converted to text",{
  expect_error(extract_delim_linear(list(a=1, b="car"), "l", "r"))
  expect_error(extract_delim_linear(c(1,2), "l", "r"))
})
test_that("extract_delim_linear returns valid output",{
  out = extract_delim_linear("// a test \n", "//", "\n")
  expect_equal(names(out), c("out", "x")) ## I really need to find better names
  expect_true(is.data.frame(out$out))
  expect_type(out$x, 'character')
  expect_equal(names(out$out), c(".left", ".right", ".sym", "text"))
})
test_that("extract_delim_linear returns valid output with no match",{
  out = extract_delim_linear("// a test \n", "x", "y")
  expect_equal(names(out), c("out", "x")) ## I really need to find better names
  expect_true(is.data.frame(out$out))
  expect_type(out$x, 'character')
  expect_equal(names(out$out), c(".left", ".right", ".sym", "text"))
})

test_that("extract_delim_linear correctly pulls out patterns",{
  input = "// a test \n  int x = 1;"
  output = extract_delim_linear(input, "//", "\n")
  expect_equal(nchar(input), nchar(output$x))
  expect_true(grepl(output$out$text, input, fixed = TRUE))
})
# There should be some more tests for this one, probably

test_that("extract_all_delims_linear accepts both types of input",{
  txt = "// abcde \n int x = 1;"
  expect_equal(
    extract_all_delims_linear(txt, "//", "\n"),
    extract_all_delims_linear(list(out = list(), x = txt), "//", "\n"))
})
test_that("extract_all_delims_linear gets multiple inputs?", {
  txt = c("// example 1 \n int N = 10;",
          "vector[N] a; // this is a comment \n")
  singles = lapply(txt, extract_delim_linear, .left = "//", .right = "\n")
  single_df = rbind(singles[[1]]$out, singles[[2]]$out)
  single_x = collapse_lines(c(singles[[1]]$x, singles[[2]]$x))
  out = extract_all_delims_linear(txt, "//", "\n")
  expect_equal(out$out$text, single_df$text)
  expect_equal(out$x, single_x)
})

test_that("extract sequences fails if .lefts and .rights are unmatched", {
  expect_error(extract_sequence("", .lefts = c("a", "b", "c"), .rights = c("a", "b", "c", "d")))
})
test_that("extract_sequences works", {
  test_seq_template = "//{sq1}\n int {sq3} /*{sq2}*/;"
  test_seq = as.character(glue::glue_data(
    list(sq1 = "abcd", sq2 = "ghfi", sq3 = "x = 1"), test_seq_template))
  ext_seq = extract_sequence(test_seq, c("//", "/*", "int"), c("\n", "*/", ";"))
  expect_equal(ext_seq$x, null_string(test_seq))
  # There should be more here...
})
test_that("extract_sequence has right output structure w/ no matches", {
  expect_equal(names(extract_sequence("abcde", "g", "f")),
               c("out", "x"))
  # extract_sequence("abcde", "b", "d")
})

test_that("remove_empty_strings does its job", {
  txt = c("a", "b", "", "c", "d", "", "", "e")
  expect_equal(remove_empty_strings(txt), letters[1:5])
})



test_that("glue_args works", {
  txt = "test [a + b]"
  glued =glue(txt, a = 1, b = 0, .open = "[", .close = "]")
  ctrl = control = list(.open = "[", .close = "]")
  expect_equal(
    glue_args(what = txt, args = list(a = 1, b = 0),  ctrl),
    glued)
  # test dot args
  expect_equal(
    glue_args(txt, args = list(a = 1), ctrl, b = 0), glued)
})



test_fun = structure(function(a, b, ...) {
  glue("{a}, {b}")
}, class = c("stan_macro", "function"))
wrap_fun = wrap_quote(test_fun)

test_that("test wrap_quote", {
  expect_equal(wrap_fun(a = zed[1], b = pm), test_fun("zed[1]","pm"))
})

test_that("quote_macros works", {
  the_list = list(a = test_fun, b = "three",
                  c = structure(identity, class = c("stan_macro", "function")))
  quoted_list = quote_macros(the_list)
  expect_equal(glue_data(quoted_list, "{a(one, two)}, and {b} makes {c(six)}"),
               "one, two, and three makes six")
})

test_that("glue_args can handle quoting", {
  txt = "{{func(alpha2, bogon)}}"
  expect_error( # not quoted
    glue_args(what = txt, func =  test_fun))
  expect_equal(
    glue_args(what = txt, func = test_fun, .quote = TRUE),
    "alpha2, bogon")
})
