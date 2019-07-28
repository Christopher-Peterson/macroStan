#' @export
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
