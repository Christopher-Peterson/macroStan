// Multivariate NCP example
use macros {
  alpha_beta = ncp_mv(N_groups, D + 1,
          location = append_row([mu_alpha]', mu_beta),
          scale = append_row([tau_alpha]', tau_beta),
          L_corr = L_omega);
  eta = ncp_mv_linpred(alpha_beta, x, group);
}
data{
  int N;
  int N_groups;
  int<lower=1,upper=N_groups> group[N];
  int D; // number of predictors
  matrix[N, D] x;
  vector[N] y;
}
parameters {
  real<lower=0> sigma;
  real<lower=0> tau_alpha;
  vector<lower=0>[D] tau_beta;
  real mu_alpha;
  vector[D] mu_beta;
  cholesky_factor_corr[D+1] L_omega;
}
model {
  // priors
  target += student_t_lpdf(sigma | 7, 0, 5) +
            student_t_lpdf(tau_alpha | 7, 0, 5) +
            student_t_lpdf(tau_beta | 7, 0, 2.5) +
            normal_lpdf(mu_alpha | 0, 10) +
            normal_lpdf(mu_beta | 0, 5) +
            lkj_corr_cholesky_lpdf(L_omega | 2);
  // Likelihood
  target += normal_lpdf(y | eta, sigma);
}
