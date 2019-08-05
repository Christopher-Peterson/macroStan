// Non-central parameterization
macro args {
  N_group; // int;
  N_coef;  // int; number of correlated parameters
  location = mu; // vector[N_coef]; the mean parameters
  scale = tau;  // vector[N_coef]; tau/standard deviation
  L_corr = L_omega; // cholesky_factor_corr[N_coef]
  value = beta; // matrix[N_group, N_coef]
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
