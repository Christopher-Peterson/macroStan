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
