macroStan
================

R package for defining text macros in Stan for flexible code reuse.

macroStan provides a method to define reuseable, parameterized chunks of
code and insert them into the appropriate locations of a stan file. The
package was inspired by (and uses) the `glue` package.

## Example: Varying intercept non-centered parameterization

Macros are defined as a stan file with an extra `macro args` block at
the top. Each argument is declared on a separate line ending with a
semicolon; default values can be optionally provided. To use a macro, a
`use macros` block is included with a stan file, with each listing a
macro and defining its arguments.

Here’s a macro for the non-centered parameterization of a varying
intercept model, paired with a simple “scaffold” model to use it with.

**Macro:**

``` stan
// Non-central parameterization
macro args {
  N_group; // int;
  location = mu; // vector[N_coef]; the mean parameters
  scale = tau;  // vector[N_coef]; tau/standard deviation
  value; // matrix[N_group, N_coef]
}
parameters{
  // ncp_mv parameters
  vector[{|N_group|}] z_{|value|};
}
transformed parameters{
 // ncp_mv regression coefs
 vector[{|N_group|}] {|value|} = mu + z_{|value|} * tau;
}
model{
  // ncp_mv prior
  target += std_normal_lpdf(z_{|value|});
}
```

This macro has the arguments `N_group`, `location`, `scale`, and
`value`. Code in the remaining blocks has been written with argument
tags (e.g., `{|N_group|}`), which are replaced with the argument’s value
when parsed.

**Scaffold:**

``` stan
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
```

The macro is invoked with `alpha = ncp_simple(N_groups, mu, tau);`. The
parser will match the provided arguments by name or position. If the
macro is called as part of an assignment, the left hand side is treated
as the `value` argument (if the macro has one). In the macros I’ve
included in this document, the `value` argument corresponds to a
transformed parameter or local model variable that is primary output of
the macro; the assignment syntax highlights that the value should be
useable in other parts of the stan program.

This call thus defines the arguments `N_group` as `N_groups`, `location`
as `mu`, `scale` as `tau`, and `value` as `alpha`. Once the arguments
are defined, argument tags are replaced with their new definitions
(e.g., `z_{|value|} * {|scale|}` becomes `z_alpha * tau`).

The parsed macro code is inserted into the calling stan file in the
appropriate program blocks. For blocks that can contain both
declarations and other statements, the macro is inserted after the last
declaration. If multiple macros are defined, they are inserted in the
order they appear in the `use macros` block.

**Parsing the macro in R:**

``` r
# macros$macro_ncp_simple and 
# scaffolds$scaffold_ncp_simple are filepaths 
# to the above stan files
out_file = tempfile(fileext = ".stan")
ncp_simple = macroStan::parse_stan_macros(
  input = scaffolds$scaffold_ncp_simple, 
  output = out_file,
  macro_files = list(ncp_simple = macros$macro_ncp_simple))
```

**Output:**

``` stan
data { 
int N;
  int N_groups;
  int<lower=1,upper=N_groups> group[N];
  vector[N] y; 
} 
parameters { 
real<lower=0> sigma;
  real<lower=0> tau;
  real<lower=0> mu;

  // ncp_mv parameters
  vector[N_groups] z_alpha; 
} 
transformed parameters { 
// ncp_mv regression coefs
vector[N_groups] alpha = mu + z_alpha * tau; 
} 
model { 
// ncp_mv prior
  target += std_normal_lpdf(z_alpha);

  // priors
  target += student_t_lpdf(sigma | 7, 0, 5) +
            student_t_lpdf(tau | 7, 0, 5) +
            normal_lpdf(mu | 0, 10);
  // Likelihood
  target += normal_lpdf(y | alpha[group], sigma); 
} 
```

## Example: Horseshoe prior

As a more complicated example, here is the regularized horseshoe prior,
adapted from the implementation in `brms`.

**Macro:**

``` stan
macro args {
  N_local = N_group;
  value = beta_hs;
}
functions {
  // horseshoe computation
  vector horseshoe(vector zb, vector[] local, real[] global,
                 real scale_global, real c2) {
    int K = rows(zb);
    vector[K] lambda = local[1] .* sqrt(local[2]);
    vector[K] lambda2 = square(lambda);
    real tau = global[1] * sqrt(global[2]) * scale_global;
    vector[K] lambda_tilde = sqrt(c2 * lambda2 ./ (c2 + tau^2 * lambda2));
    return zb .* lambda_tilde * tau;
  }
}
data {
  // data for horseshoe prior
  real<lower=0> hs_df_{|value|};
  real<lower=0> hs_df_global_{|value|};  // global degrees of freedom
  real<lower=0> hs_df_slab_{|value|};  // slab degrees of freedom
  real<lower=0> hs_scale_global_{|value|};  // global prior scale
  real<lower=0> hs_scale_slab_{|value|};  // slab prior scale",
  }
parameters{
  // horseshoe shrinkage parameters, global
  real<lower=0> hs_global_{|value|}[2];  // global shrinkage parameters
  real<lower=0> hs_c2_{|value|};  // slab regularization parameter
  // local parameters for horseshoe
  vector[{|N_local|}] hs_z_{|value|};
  vector<lower=0>[{|N_local|}] hs_local_{|value|}[2];

}
transformed parameters{
 // horseshoe regression coefs
  vector[{|N_local|}] {|value|} =
    horseshoe(hs_z_{|value|}, hs_local_{|value|},
    hs_global_{|value|}, hs_scale_global_{|value|},
    hs_scale_slab_{|value|}^2 * hs_c2_{|value|}  );
}
model{
 // horseshoe prior, global
  target += std_normal_lpdf(hs_global_{|value|}[1]) - 1 * log(0.5) +
            inv_gamma_lpdf(hs_global_{|value|}[2] |
              0.5 * hs_df_global_{|value|},0.5 * hs_df_global_{|value|} ) +
            inv_gamma_lpdf(hs_c2_{|value|} |
              0.5 * hs_df_slab_{|value|},0.5 * hs_df_slab_{|value|} );
  //horseshoe prior, local
  target += std_normal_lpdf(hs_z_{|value|}) +
            std_normal_lpdf(hs_local_{|value|}[1]) -  {|N_local|} * log(0.5) +
            inv_gamma_lpdf(hs_local_{|value|}[2] |
               0.5 * hs_df_{|value|}, 0.5 * hs_df_{|value|});
}
```

**Scaffold:**

``` stan
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
```

Note that the macro was called with `value = beta`, and that `beta` is
used in the model block to connect the assembled horseshoe prior
coefficients to the linear predictor.

**Parsing:**

``` r
# once again, input and macro_files are paths that link to 
# the above definitions
out_file = tempfile(fileext = ".stan")
horseshoe = macroStan::parse_stan_macros(
  input = scaffolds$scaffold_hs, 
  output = out_file,
  macro_files = list(horseshoe = macros$macro_hs))
```

**Output:**

``` stan
functions { 
// horseshoe computation
  vector horseshoe(vector zb, vector[] local, real[] global,
                 real scale_global, real c2) {
    int K = rows(zb);
    vector[K] lambda = local[1] .* sqrt(local[2]);
    vector[K] lambda2 = square(lambda);
    real tau = global[1] * sqrt(global[2]) * scale_global;
    vector[K] lambda_tilde = sqrt(c2 * lambda2 ./ (c2 + tau^2 * lambda2));
    return zb .* lambda_tilde * tau;
  } 
} 
data { 
int N;
  int D;
  matrix[N,D] x;
  vector[N] y;

// data for horseshoe prior
real<lower=0> hs_df_beta;
real<lower=0> hs_df_global_beta;  // global degrees of freedom
real<lower=0> hs_df_slab_beta;  // slab degrees of freedom
real<lower=0> hs_scale_global_beta;  // global prior scale
real<lower=0> hs_scale_slab_beta;  // slab prior scale", 
} 
parameters { 
real<lower=0> sigma;
  real alpha;

  // horseshoe shrinkage parameters, global
  real<lower=0> hs_global_beta[2];  // global shrinkage parameters
  real<lower=0> hs_c2_beta;  // slab regularization parameter
  // local parameters for horseshoe
  vector[D] hs_z_beta;
  vector<lower=0>[D] hs_local_beta[2]; 
} 
transformed parameters { 
// horseshoe regression coefs
 vector[D] beta =
   horseshoe(hs_z_beta, hs_local_beta,
   hs_global_beta, hs_scale_global_beta,
   hs_scale_slab_beta^2 * hs_c2_beta  ); 
} 
model { 
vector[N] eta = alpha + x * beta ;

 // horseshoe prior, global
  target += std_normal_lpdf(hs_global_beta[1]) - 1 * log(0.5) +
            inv_gamma_lpdf(hs_global_beta[2] |
              0.5 * hs_df_global_beta,0.5 * hs_df_global_beta ) +
            inv_gamma_lpdf(hs_c2_beta |
              0.5 * hs_df_slab_beta,0.5 * hs_df_slab_beta );
  //horseshoe prior, local
  target += std_normal_lpdf(hs_z_beta) +
            std_normal_lpdf(hs_local_beta[1]) -  D * log(0.5) +
            inv_gamma_lpdf(hs_local_beta[2] |
               0.5 * hs_df_beta, 0.5 * hs_df_beta);

  target += normal_lpdf(y | eta, sigma); 
} 
```

## Multivariate Noncentered Parameterizations:

This is a version of the multivariate NCP (see the [Stan User’s
Guide 21.7](https://mc-stan.org/docs/2_18/stan-users-guide/reparameterization-section.html)
for more details). In this example, we use two macros: The first defines
coefficients:

**Primary Macro:**

``` stan
// Non-central parameterization
macro args {
  N_group; // int;
  N_coef;  // int; number of correlated parameters
  location = mu; // vector[N_coef]; the mean parameters
  scale = tau;  // vector[N_coef]; tau/standard deviation
  L_corr = L_omega; // cholesky_factor_corr[N_coef]
  value = coef; // matrix[N_group, N_coef]
}
parameters{
  // ncp_mv parameters
  matrix[{|N_group|}, {|N_coef|}] z_{|value|};
}
transformed parameters{
 // ncp_mv regression coefs
 matrix[{|N_group|}, {|N_coef|}] {|value|} =
 rep_matrix( {|location|}', {|N_group|}) +
   z_{|value|} * diag_pre_multiply({|scale|}, {|L_corr|} )';
}
model{
  // ncp_mv prior
  target += std_normal_lpdf(to_vector(z_{|value|}));
}
```

The second combines the coefficients with the data into a linear
predictor.  
**Helper
Macro:**

``` stan
// Non-central parameterization helper macro; assembles linear predictor from x and coeficients
  // While this could easily be included as a function in macro_mv,
  // I am using it as an example of using multiple macros at once
macro args {
  coefs; // matrix[N_group, N_coef], produced by ncp_mv macro
  x; // matrix[N, N_coef - 1], predictors
  group; // int[N], index of group membership
  value = eta; // vector, linear predictor
}
functions {
  vector get_linpred_ncp_mv(matrix coefs, matrix x, int[] group) {
    // Assume x has no intercept row
    int N = rows(x);
    int D = cols(x);
    vector[N] eta;
    vector[N] alpha = col(coefs, 1);
    matrix[N, D] beta = block(coefs, 1,2,N,D);
    return alpha[group] + rows_dot_product(x, beta[group]);
  }
}
model {
  vector[rows( {|x|} )] {|value|} =
    get_linpred_ncp_mv({|coefs|}, {|x|}, {|group|});
}
```

The helper macro could easily be combined with the primary one (or used
as an independent function in the scaffold), but it allowed for an easy
way to demonstrate combining macros.

**Scaffold:**

``` stan
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
```

Note that the `value` argument of the first macro is a different
argument for the second macro.

**Parsing:**

``` r
# once again, input and macro_files are paths that link to 
# the above definitions
out_file = tempfile(fileext = ".stan")
ncp_mv = macroStan::parse_stan_macros(
  input = scaffolds$scaffold_ncp, 
  output = out_file,
  macro_files = list(ncp_mv = macros$macro_ncp,
                     ncp_mv_linpred = macros$macro_ncp_lp))
```

**Output:**

``` stan
functions { 
vector get_linpred_ncp_mv(matrix coefs, matrix x, int[] group) {
    // Assume x has no intercept row
    int N = rows(x);
    int D = cols(x);
    vector[N] eta;
    vector[N] alpha = col(coefs, 1);
    matrix[N, D] beta = block(coefs, 1,2,N,D);
    return alpha[group] + rows_dot_product(x, beta[group]);
  } 
} 
data { 
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

  // ncp_mv parameters
  matrix[N_groups, D + 1] z_alpha_beta; 
} 
transformed parameters { 
// ncp_mv regression coefs
matrix[N_groups, D + 1] alpha_beta =
rep_matrix( append_row([mu_alpha]', mu_beta)', N_groups) +
  z_alpha_beta * diag_pre_multiply(append_row([tau_alpha]', tau_beta), L_omega )'; 
} 
model { 
vector[rows( x )] eta =
  get_linpred_ncp_mv(alpha_beta, x, group);
  // ncp_mv prior
  target += std_normal_lpdf(to_vector(z_alpha_beta));


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
```

## Horseshoe + NCP

Finally, let’s combine the horseshoe and the multivariate NCP together.
The macro files are already defined, but we need a new scaffold.

**Scaffold:**

``` stan
// Multivariate NCP example, with horseshoe
use macros {
  mu_beta = horseshoe(D);
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
```

**Parsing:**

``` r
# once again, input and macro_files are paths that link to 
# the above definitions
out_file = tempfile(fileext = ".stan")
ncp_mv_hs = macroStan::parse_stan_macros(
  input = scaffolds$scaffold_ncp_hs, 
  output = out_file,
  macro_files = list(horseshoe = macros$macro_hs,
                     ncp_mv = macros$macro_ncp,
                     ncp_mv_linpred = macros$macro_ncp_lp))
```

**Output:**

``` stan
functions { 
// horseshoe computation
  vector horseshoe(vector zb, vector[] local, real[] global,
                 real scale_global, real c2) {
    int K = rows(zb);
    vector[K] lambda = local[1] .* sqrt(local[2]);
    vector[K] lambda2 = square(lambda);
    real tau = global[1] * sqrt(global[2]) * scale_global;
    vector[K] lambda_tilde = sqrt(c2 * lambda2 ./ (c2 + tau^2 * lambda2));
    return zb .* lambda_tilde * tau;
  }
  vector get_linpred_ncp_mv(matrix coefs, matrix x, int[] group) {
    // Assume x has no intercept row
    int N = rows(x);
    int D = cols(x);
    vector[N] eta;
    vector[N] alpha = col(coefs, 1);
    matrix[N, D] beta = block(coefs, 1,2,N,D);
    return alpha[group] + rows_dot_product(x, beta[group]);
  } 
} 
data { 
int N;
  int N_groups;
  int<lower=1,upper=N_groups> group[N];
  int D; // number of predictors
  matrix[N, D] x;
  vector[N] y;

// data for horseshoe prior
real<lower=0> hs_df_mu_beta;
real<lower=0> hs_df_global_mu_beta;  // global degrees of freedom
real<lower=0> hs_df_slab_mu_beta;  // slab degrees of freedom
real<lower=0> hs_scale_global_mu_beta;  // global prior scale
real<lower=0> hs_scale_slab_mu_beta;  // slab prior scale", 
} 
parameters { 
real<lower=0> sigma;
  real<lower=0> tau_alpha;
  vector<lower=0>[D] tau_beta;
  real mu_alpha;
  cholesky_factor_corr[D+1] L_omega;

  // horseshoe shrinkage parameters, global
  real<lower=0> hs_global_mu_beta[2];  // global shrinkage parameters
  real<lower=0> hs_c2_mu_beta;  // slab regularization parameter
  // local parameters for horseshoe
  vector[D] hs_z_mu_beta;
  vector<lower=0>[D] hs_local_mu_beta[2];
  // ncp_mv parameters
  matrix[N_groups, D + 1] z_alpha_beta; 
} 
transformed parameters { 
// horseshoe regression coefs
 vector[D] mu_beta =
   horseshoe(hs_z_mu_beta, hs_local_mu_beta,
   hs_global_mu_beta, hs_scale_global_mu_beta,
   hs_scale_slab_mu_beta^2 * hs_c2_mu_beta  );
// ncp_mv regression coefs
matrix[N_groups, D + 1] alpha_beta =
rep_matrix( append_row([mu_alpha]', mu_beta)', N_groups) +
  z_alpha_beta * diag_pre_multiply(append_row([tau_alpha]', tau_beta), L_omega )'; 
} 
model { 
vector[rows( x )] eta =
  get_linpred_ncp_mv(alpha_beta, x, group);
 // horseshoe prior, global
  target += std_normal_lpdf(hs_global_mu_beta[1]) - 1 * log(0.5) +
            inv_gamma_lpdf(hs_global_mu_beta[2] |
              0.5 * hs_df_global_mu_beta,0.5 * hs_df_global_mu_beta ) +
            inv_gamma_lpdf(hs_c2_mu_beta |
              0.5 * hs_df_slab_mu_beta,0.5 * hs_df_slab_mu_beta );
  //horseshoe prior, local
  target += std_normal_lpdf(hs_z_mu_beta) +
            std_normal_lpdf(hs_local_mu_beta[1]) -  D * log(0.5) +
            inv_gamma_lpdf(hs_local_mu_beta[2] |
               0.5 * hs_df_mu_beta, 0.5 * hs_df_mu_beta);
  // ncp_mv prior
  target += std_normal_lpdf(to_vector(z_alpha_beta));


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
```
