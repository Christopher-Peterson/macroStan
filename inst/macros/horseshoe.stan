macro args {
  name;
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
  real<lower=0> hs_df_{|name|};
  real<lower=0> hs_df_global_{|name|};  // global degrees of freedom
  real<lower=0> hs_df_slab_{|name|};  // slab degrees of freedom
  real<lower=0> hs_scale_global_{|name|};  // global prior scale
  real<lower=0> hs_scale_slab_{|name|};  // slab prior scale",
  }
parameters{
  // horseshoe shrinkage parameters, global
  real<lower=0> hs_global_{|name|}[2];  // global shrinkage parameters
  real<lower=0> hs_c2_{|name|};  // slab regularization parameter
  // local parameters for horseshoe
  vector[{|N_local|}] hs_z_{|name|};
  vector<lower=0>[{|N_local|}] hs_local_{|name|}[2];

}
transformed parameters{
 // horseshoe regression coefs
  vector[{|N_local|}] {|value|} =
    horseshoe(hs_z_{|name|}, hs_local_{|name|},
    hs_global_{|name|}, hs_scale_global_{|name|},
    hs_scale_slab_{|name|}^2 * hs_c2_{|name|}  );
}
model{
 // horseshoe prior, global
  target += std_normal_lpdf(hs_global_{|name|}[1]) - 1 * log(0.5) +
            inv_gamma_lpdf(hs_global_{|name|}[2] |
              0.5 * hs_df_global_{|name|},0.5 * hs_df_global_{|name|} ) +
            inv_gamma_lpdf(hs_c2_{|name|} |
              0.5 * hs_df_slab_{|name|},0.5 * hs_df_slab_{|name|} );
  //horseshoe prior, local
  target += std_normal_lpdf(hs_z_{|name|}) +
            std_normal_lpdf(hs_local_{|name|}[1]) -  {|N_local|} * log(0.5) +
            inv_gamma_lpdf(hs_local_{|name|}[2] |
               0.5 * hs_df_{|name|}, 0.5 * hs_df_{|name|});
}
