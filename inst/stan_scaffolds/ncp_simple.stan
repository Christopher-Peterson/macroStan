// Multivariate NCP example
use macros {
  alpha = ncp_simple(N_groups, location = mu, scale = tau);
}
data{
  int N;
  int N_groups;
  int<lower=1,upper=N_groups> group[N];
  vector[N] y;
}
parameters {
  real<lower=0> sigma;
  real<lower=0> tau;
  real<lower=0> mu;
}
model {
  // priors
  target += student_t_lpdf(sigma | 7, 0, 5) +
            student_t_lpdf(tau | 7, 0, 5) +
            normal_lpdf(mu | 0, 10);
  // Likelihood
  target += normal_lpdf(y | alpha[group], sigma);
}
