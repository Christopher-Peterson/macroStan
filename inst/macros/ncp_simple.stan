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
