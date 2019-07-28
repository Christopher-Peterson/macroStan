macroStan
================

R package for defining text macros in Stan for flexible code reuse.

macroStan provides a method to define reuseable, parameterized chunks of
code and insert them into the appropriate locations of a stan file. The
package was inspired by (and uses) the `glue` package.

## Example: Non-centered parameterization

As a simple example, we’ll define a macro for the non-centered
parameterization of a hierarchical parameter.

``` r
stan_macro_ncp = define_stan_macro(
  .args = alist(name="alpha", mu ="mu", sd ="tau", size ="N_groups"),
  coef = "{{name}}",
  parms = "// Non-central parameterization of {{name}}
  vector[{{size}}] {{name}}_z;",
  tp1 = "// Non-central parameterization of {{name}}
  vector[{{size}}] {{name}} = {{name}}_z * {{sd}} + {{mu}};",
  prior = "// Non-central parameterization of {{name}}
  target += std_normal_lpdf({{name}}_z);"
)
```

This defines a macro with the arguments `name`, `mu`, `sd`, and `size`.
The macro contains a number of sections (`coef`, `parms`, `tp1`,
`prior`), which contain snippets of stan code. The macro’s arguments are
referenced in the sections with double curly braces. Let’s use this
macro to setup a hierarchical intercept with specific
parameters:

``` r
alpha_ncp = stan_macro_ncp(name = "alpha", mu = "mu", sd = "tau", size = "N_groups")
```

Now the parameters snippet (`alpha_ncp$parm`) is:

    ## // Non-central parameterization of alpha
    ## vector[N_groups] alpha_z;

Once a macro has been defined, a stan model can be written to use it

``` stan
// Simple non-central parameterization example
// Required macros:
  // alpha_ncp (stan_macro_ncp)
data {
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
  $ alpha_ncp$tp1
}
model {
  // Prior distributions
  $ alpha_ncp$prior
  target += student_t_lpdf([sigma, tau] | 7, 0, 5) ;

  target += normal_lpdf( y | {{ alpha_ncp$coef }}[group] , sigma);
}
```

There are two ways of inserting macros into a stan file: whole line
replacement, which begins with a `$`, and in-line replacement, which
uses the same `{{ }}` as the macro definitions. To put them together,
use `parse_stan_macros`.

``` r
output_file = tempfile(fileext = ".stan")
# input_file corresponds to the above stan model
parse_stan_macros(input = input_file, out_file = output_file,
                  alpha_ncp = alpha_ncp)
```

Which produces the following stan file:

``` stan
// Simple non-central parameterization example
// Required macros:
  // alpha_ncp (stan_macro_ncp)
data {
  int N;
  int N_groups;
  int<lower=1,upper=N_groups> group[N];
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
  real<lower=0> tau;
// Non-central parameterization of alpha
vector[N_groups] alpha_z;
}
transformed parameters {
// Non-central parameterization of alpha
vector[N_groups] alpha = alpha_z * tau + mu;
}
model {
  // Prior distributions
// Non-central parameterization of alpha
target += std_normal_lpdf(alpha_z);
  target += student_t_lpdf([sigma, tau] | 7, 0, 5) ;

  target += normal_lpdf( y | alpha[group] , sigma);
}
```

## Example 2: Horseshoe priors

This adapts the horseshoe prior used in `brms`.

``` r
stan_macro_horseshoe = define_stan_macro(
  .args = alist(name=1, N_local = "D"),
  coef = "b_{{name}}",
  functions = "// horseshoe computation
  vector horseshoe(vector zb, vector[] local, real[] global,
                 real scale_global, real c2) {
    int K = rows(zb);
    vector[K] lambda = local[1] .* sqrt(local[2]);
    vector[K] lambda2 = square(lambda);
    real tau = global[1] * sqrt(global[2]) * scale_global;
    vector[K] lambda_tilde = sqrt(c2 * lambda2 ./ (c2 + tau^2 * lambda2));
    return zb .* lambda_tilde * tau;
  }",
  data = "  // data for horseshoe prior
  real<lower=0> hs_df_{{name}};
  real<lower=0> hs_df_global_{{name}};  // global degrees of freedom
  real<lower=0> hs_df_slab_{{name}};  // slab degrees of freedom
  real<lower=0> hs_scale_global_{{name}};  // global prior scale
  real<lower=0> hs_scale_slab_{{name}};  // slab prior scale",
  parms = "  // horseshoe shrinkage parameters, global
  real<lower=0> hs_global_{{name}}[2];  // global shrinkage parameters
  real<lower=0> hs_c2_{{name}};  // slab regularization parameter
  // local parameters for horseshoe
  vector[{{N_local}}] hs_z_{{name}};
  vector<lower=0>[{{N_local}}] hs_local_{{name}}[2];",
  tp1 = glue_args(" // horseshoe regression coefs
  vector[{{N_local}}] b_{{name}} = horseshoe({{hs_args}});",
     N_local = "{{N_local}}", name = "{{name}}",
     hs_args = paste("hs_z_{{name}}, hs_local_{{name}}",
       "hs_global_{{name}}", "hs_scale_global_{{name}}",
       "hs_scale_slab_{{name}}^2 * hs_c2_{{name}}", sep = ", ") ),
  prior =  " // horseshoe prior, global
  target += std_normal_lpdf(hs_global_{{name}}[1]) - 1 * log(0.5) +
            inv_gamma_lpdf(hs_global_{{name}}[2] |
              0.5 * hs_df_global_{{name}},0.5 * hs_df_global_{{name}} ) +
            inv_gamma_lpdf(hs_c2_{{name}} |
              0.5 * hs_df_slab_{{name}},0.5 * hs_df_slab_{{name}} );
  target += std_normal_lpdf(hs_z_{{name}}) +
            std_normal_lpdf(hs_local_{{name}}[1]) -  {{N_local}} * log(0.5) +
            inv_gamma_lpdf(hs_local_{{name}}[2] |
               0.5 * hs_df_{{name}}, 0.5 * hs_df_{{name}});"
  )
```

`input_file_hs:`

``` stan
// Horseshoe prior example
// Required macros:
  // hs_betas (stan_macro_horseshoe)
functions{
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
  $ hs_betas$tp1
}
model {
  vector[N] eta = alpha + x * {{ hs_betas$coef }} ;
  $ hs_betas$prior

  target += normal_lpdf(y | eta, sigma);
}
```

``` r
output_file_hs = tempfile(fileext = ".stan")
parse_stan_macros(input_file_hs, output_file_hs, 
                  hs_betas = stan_macro_horseshoe("1", "D"))
```

``` stan
// Horseshoe prior example
// Required macros:
  // hs_betas (stan_macro_horseshoe)
functions{
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
real<lower=0> hs_df_1;
real<lower=0> hs_df_global_1;  // global degrees of freedom
real<lower=0> hs_df_slab_1;  // slab degrees of freedom
real<lower=0> hs_scale_global_1;  // global prior scale
real<lower=0> hs_scale_slab_1;  // slab prior scale
}
parameters {
  real<lower=0> sigma;
  real alpha;
// horseshoe shrinkage parameters, global
real<lower=0> hs_global_1[2];  // global shrinkage parameters
real<lower=0> hs_c2_1;  // slab regularization parameter
// local parameters for horseshoe
vector[D] hs_z_1;
vector<lower=0>[D] hs_local_1[2];
}
transformed parameters {
// horseshoe regression coefs
vector[D] b_1 = horseshoe(hs_z_1, hs_local_1, hs_global_1, hs_scale_global_1, hs_scale_slab_1^2 * hs_c2_1);
}
model {
  vector[N] eta = alpha + x * b_1 ;
// horseshoe prior, global
target += std_normal_lpdf(hs_global_1[1]) - 1 * log(0.5) +
          inv_gamma_lpdf(hs_global_1[2] |
            0.5 * hs_df_global_1,0.5 * hs_df_global_1 ) +
          inv_gamma_lpdf(hs_c2_1 |
            0.5 * hs_df_slab_1,0.5 * hs_df_slab_1 );
target += std_normal_lpdf(hs_z_1) +
          std_normal_lpdf(hs_local_1[1]) -  D * log(0.5) +
          inv_gamma_lpdf(hs_local_1[2] |
             0.5 * hs_df_1, 0.5 * hs_df_1);

  target += normal_lpdf(y | eta, sigma);
}
```
