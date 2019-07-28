ncp_code = readLines("../../inst/stan_scaffolds/macro_ncp.stan")
alpha = stan_macro_ncp("alpha", "mu", "tau", "N_groups")
test_that("ncp macro works",{
  expect_type(parse_stan_macros(ncp_code, alpha_ncp = alpha), "character")

})
