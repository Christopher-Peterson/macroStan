// Horseshoe prior example
use macros {
  beta = horseshoe(D);
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
model {
  vector[N] eta = alpha + x * beta ;
  target += normal_lpdf(y | eta, sigma);
}
