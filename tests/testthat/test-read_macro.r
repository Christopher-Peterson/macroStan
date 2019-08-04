test_that("get_macro_args works", {
  input = list(`macro args` = c("
  beta = x;
  // comment, ignore
  prior = normal_lpdf(y | mu, sigma);
  delta = [1,2]';
  value;
  "))
  expec = rlang::parse_exprs(c(
    "beta = \"x\"",
    "prior = \"normal_lpdf(y | mu, sigma)\"",
    "delta = \"[1,2]'\"",
    "value"))
  out = macroStan:::get_macro_args(input)
  # If this gets worse, try using a map function
  lapply(seq_along(expec), function(i)
    expect_identical(out[[i]], expec[[i]]))
  })
test_that("get_arg_names works",{
  arg_names = rlang::parse_exprs(c(
    "a", "b = 1", "c = foo(bar)", "d"))
  expect_equal(get_arg_names(arg_names), letters[1:4])
  expect_error(get_arg_names("foo(bar)")) # not an assignment
})
test_that("validate_macro_args catches errors", {
  good_txt = list(
  `macro args` = "
    alpha;
    beta = 1;
    value = val",
  data = "
  int N;
  real size_{| alpha |}")
  bad_txt = good_txt
  bad_txt$parameters = "
  real {| theta |};"
  args1 = macroStan:::get_macro_args(good_txt)
  args2 = macroStan:::get_macro_args(bad_txt)
  expect_invisible(macroStan:::validate_macro_args(good_txt, args1))
  expect_error(macroStan:::validate_macro_args(bad_txt, args2))
  })

test_that("parse_macro_formals works",{
  arg_lst = rlang::parse_exprs(c("a", "b = 1", "c = foo"))
  expect_identical(parse_macro_formals(arg_lst),
                   alist(a=, b="1", c="foo"))
})

test_file = "../../inst/macros/horseshoe.stan"
# test_file = "inst/macros/horseshoe.stan"

test_that("read_macro creates an object of the correct class",{
  out = read_macro(test_file)
  expect_equal(class(out), c("stan_macro", "function"))
  expect_equal(names(formals(out)), c("name", "N_local", "value"))
  expect_equal(formals(out)$N_local, "N_group")
  expect_equal(formals(out)$value, "beta_hs")
  expect_true(rlang::is_missing(formals(out)$name))
})
test_that("read_macro's output is in the right format",{
  macro = read_macro(test_file)
  expect_error(macro(name=)) # missig name
  block_names = c("functions", "data", "transformed data",
                  "parameters","transformed parameters",
                  "model", "generated quantities")
  out = macro(name = "test")
  expect_type(out, "list")
  expect_true(all(names(out) %in% block_names))
  two_part_blocks = names(out)[names(out) %in% block_names[c(3, 5, 6, 7)]]
  lapply(out[two_part_blocks], function(x)
    expect_equal(names(x), c("declarations", "post")))
  # check for unparsed tags
  expect_false(any(grepl("{|", unlist(out), fixed = TRUE)))
  expect_false(any(grepl("|}", unlist(out), fixed = TRUE)))
  })

test_that("get_macro_file_list recognizes real files, requries names",{
  tst = macroStan:::get_macro_file_list(list(horseshoe = test_file))
  expect_equal(names(tst), "horseshoe")
  expect_true(is_stan_macro(tst[[1]]))
  # No name
  expect_error(macroStan:::get_macro_file_list(
    list(test_file)))
  expect_error(macroStan:::get_macro_file_list(
    list(test_file, b=test_file)))
  expect_error(macroStan:::get_macro_file_list(
    list(bad = "bad.stan")))
})
