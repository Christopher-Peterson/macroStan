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
  expect_false(any(grepl("{{", unlist(sm(a = "tst")), fixed = TRUE)))
  expect_false(any(grepl("}}", unlist(sm(a = "tst")), fixed = TRUE)))
})
