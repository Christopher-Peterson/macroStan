// Horseshoe prior example
// Required macros:
  // hs_betas (stan_macro_horseshoe)
use macros {
  beta = horseshoe(name = test, D);
}
functions{
}
data {
  int N;
  int D;
  matrix[N,D] x;
  vector[N] y;
}


parameters {
  real<lower=0> sigma;
  real alpha;
}
transformed parameters {
}
model {
  vector[N] eta = alpha + x * beta ;

  target += normal_lpdf(y | eta, sigma);
}
