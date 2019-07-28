// Simple non-central parameterization example
// Required macros:
  // alpha_ncp (stan_macro_ncp)
data{
  int N;
  int N_groups;
  int<lower=1,upper=N_groups> group[N];
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
  real<lower=0> tau;
$ alpha_ncp$parms
}
transformed parameters {
  $ alpha_ncp$tparms
}
model {
  // Prior distributions
  $ alpha_ncp$prior
  target += student_t_lpdf([sigma, tau] | 7, 0, 5) ;

  target += normal_lpdf( y | {{ alpha_ncp$name }}[group] , sigma);
}
