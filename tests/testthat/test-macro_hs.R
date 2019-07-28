hs_test_code =
"functions{
  $ hs_betas$functions
}
data {
  int N;
  int D;
  matrix[N,D] x;
  vector[N] y;
  $ hs_betas$data
}
parameters {
  real<lower=0> sigma;
  real alpha;
  $ hs_betas$parms
}
transformed parameters {
  $ hs_betas$tparms
}
model {
  vector[N] eta = alpha + x * {{ hs_betas$coef }} ;
  $ hs_betas$prior

  target += normal_lpdf(y | eta, sigma);
}"

test_that("simple horseshoe model is syntactically correct", {
  expect_type(parse_stan_macros(
    hs_test_code, hs_betas = stan_macro_horseshoe("1", "D")), "character")
})
