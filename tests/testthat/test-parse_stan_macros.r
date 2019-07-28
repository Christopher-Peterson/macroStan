### glue delimiter tests ####
test_that("fix_demimiter works", {
  expect_equal(fix_delimiter(NULL, ), "{{")
  expect_equal(fix_delimiter(NULL, "}}"), "}}")
})

test_that("set_open_close works", {
  expect_equal(set_open_close(list(), "$"),
               list(.open = "{{", .close = "}}"))
})

### Create a dummy stan file ####
test_stan_code1 ={
"data{
  int N;
  vector[N] x;
  vector[N] y;
}
parameters{
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  alpha ~ normal(0,10);
  beta ~ normal(0,5);
  y ~ normal(alpha + beta * x, sigma);
}"}
### import stan tests ####
test_stan_code1_split = unlist(strsplit(test_stan_code1, "\n"))

test_stan_file = tempfile(fileext = ".stan")
writeLines(test_stan_code1, test_stan_file)

test_that("get_input_code read in a stan file",{
  expect_equal(get_input_code(test_stan_file), test_stan_code1_split)
})
test_that("get_input_code can format code as a character vector",{
  expect_equal(get_input_code(test_stan_code1), test_stan_code1_split)
})
# Nothing here requires the stan code to actually be stan code; this is tested later

#### define line macro wrap code ####
wlm_code = {get_input_code(
"data {
  $data
}
parameters {
  vector[{{N_pars}}] {{coef}};
}
model{
  $priors
  target += {{likelihood(
             theta[1], theta[2])}};
}")}
### Line macro wrapping: ####
wlm_out = wrap_line_macros(wlm_code)
test_that("wrap_line_macros works", {
  expect_equal(wlm_out[2], "{{data}}")
  expect_equal(wlm_out[5], wlm_code[5]) # doesn't replace already wrapped stuff
})
### parse_stan_macros ####
test_macro_list = list(
  data =
  "int N;
   int Y[N];
   vector[N] x;",
  coef = "theta",
  N_pars = "2",
  priors = "theta ~ normal(0, [3, 2]');")
likelihood_fun = structure(function(alpha = "theta[1]", beta = "theta[2]") {
  glue::glue("bernoulli_logit_lpmf(Y|{alpha} + {beta} * x)")},
  class = c("stan_macro", "function"))

# can generate
# For these tests, we should define a macro, otherwise this won't work

parsed_txt = parse_stan_macros(wlm_code, NA,
                               .macro_list = test_macro_list,
                               likelihood = likelihood_fun)

test_that("parse_stan_macros can generate valid stan code",{
  # This should fail because alpha isn't defined
  # It should return a character vector
  expect_type(parsed_txt, "character")
})
test_out_file = tempfile(fileext = ".stan")

written_file = parse_stan_macros(wlm_code, test_out_file,
                  .macro_list = test_macro_list,
                  likelihood = likelihood_fun)

test_that("parse_stan_macros can output a file", {
  expect_equal(parsed_txt, glue::glue_collapse(readLines(test_out_file), "\n"))
  expect_equal(written_file, test_out_file)
})
