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
