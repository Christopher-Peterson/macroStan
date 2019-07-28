hs_test_code = readLines("inst/stan_scaffolds/macro_hs.stan")

test_that("simple horseshoe model is syntactically correct", {
  expect_type(parse_stan_macros(
    hs_test_code, hs_betas = stan_macro_horseshoe("1", "D")), "character")
})
