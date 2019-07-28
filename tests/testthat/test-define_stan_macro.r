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

# define some objects for testing
test_args = alist(a = , b = 2, c = "normal")
test_text = "{b} is {c}"
sm = define_stan_macro(test_args, first = test_text,
                       second = "{a} is here")
sm_list = summary(sm)
sm_args = formals(sm)

test_that("define_stan_macro output has correct arguments", {
  # names are equal
  expect_equal(names(sm_args),
               c(names(test_args), c("...", ".section")))
  # main args are equal
  expect_equal(test_args, sm_args[1:(length(sm_args)-2)])
  # .section is correct
  expect_equal(eval(sm_args$.section, envir = environment(sm)),
               c(".all", names(sm_list)))
})

test_that("define_stan_macro's function produces valid output", {
  expect_equal(sm(a = "tst")$first, sm(a = "tst", .section = "first"))
  expect_type(sm(a = "tst"), "list")
  expect_type(sm(a = "tst", .section = "first"), "character")
  expect_type(unlist(sm(a = "tst")), "character")
  # make sure that no braces remain
  expect_false(any(grepl("{", unlist(sm(a = "tst")), fixed = TRUE)))
  expect_false(any(grepl("}", unlist(sm(a = "tst")), fixed = TRUE)))
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
  txt = "{func(alpha2, bogon)}"
  expect_error( # not quoted
    glue_args(what = txt, func =  test_fun))
  expect_equal(
    glue_args(what = txt, func = test_fun, .quote = TRUE),
    "alpha2, bogon")
})


