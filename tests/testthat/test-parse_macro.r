test_file = "../../inst/macros/horseshoe.stan"
test_scaffold = "../../inst/stan_scaffolds/horseshoe.stan"

block_names = c("functions", "data", "transformed data",
                "parameters","transformed parameters",
                "model", "generated quantities")
test_that("insert_macro_section works", {
  scaffold = "
  int x;
  x = 5;
  // opt here
  "
  macro = "
  int y;
  y = 3;"
  expec = "
  int x;
  int y;
  y = 3;
  x = 5;
  // opt here
  "
  out = insert_macro_section(scaffold, macro, pos = NA)
  expect_equal(out,expec)
  new_pos = get_line_of_tag(scaffold, "// opt here")
  expec2 = "
  int x;
  x = 5;
  int y;
  y = 3;
  // opt here
  "
  out2 = insert_macro_section(scaffold, macro, pos = new_pos);
  expect_equal(out2,expec2)
})

test_that("macros_exist works", {
  macro_lst = list(a = 1, b = 2)
  expect_true(macros_exist("a", macro_lst), 1)
  expect_error(macros_exist("c", macro_lst))
})

test_that("has_value works", {
  tst_fun = function(value){}
  expect_true(has_value(tst_fun))
  expect_false(has_value(function(){}))
})

test_that("get_macro_calls works",{
  block =list(`use macros` =
         "value = horseshoe([x,y]', name = hs_beta, N_local = [1,2]' )
      // comments are removed
      ")
  out = macroStan:::get_macro_calls(block)[[1]]
  arg_txt = as.character(as.list(out)[-1])
  as.list(out)
  expect_equal(names(out), c("", "", "name", "N_local", "value"))
  expect_equal(out[[1]], rlang::sym("horseshoe"))
  expect_equal(arg_txt, c("[x,y]'", "hs_beta", "[1,2]'", "value"))
})

test_that("parse_macro_call gives valid output?", {
  out = parse_macro_calls(
    list(`use macros` =
      "value = horseshoe(name = hs_beta, N_local = N_group)
      // comments are removed
      "),
    macro_list = list(horseshoe = read_macro(test_file)))
  expect_type(out, "list")
  expect_true(all(names(out) %in% block_names))
  expect_false(any(grepl("{|", unlist(out), fixed = TRUE)))
  expect_false(any(grepl("|}", unlist(out), fixed = TRUE)))
})
# test_file = "inst/horseshoe.stan"
# test_scaffold = "inst/stan_scaffolds/horseshoe.stan"

macro_list = list(horseshoe = macroStan:::read_macro(test_file))
the_scaffold = macroStan:::get_blocks(readLines(test_scaffold))

test_that("order_stan_macros works", {
  out = macroStan:::order_stan_macros(the_scaffold,macro_list)
# The type shoudl be a list following stan_block protocols
  expect_type(out, "list")
  expect_true(all(names(out) %in% block_names))
})
hs_parse = rlang::eval_tidy(rlang::expr(
  horseshoe("D", "beta")), data = macro_list)
hs_ordered = hs_parse
hs_ordered[["transformed parameters"]] = hs_parse[["transformed parameters"]]$declarations
hs_ordered[["model"]] = hs_parse[["model"]]$post

test_that("block_insert_functions works", {
  funs = macroStan:::block_insert_functions(the_scaffold, names(hs_parse))
  expect_true(all(names(funs) %in% block_names))
  expect_true(all(vapply(funs, rlang::is_function, TRUE)))
})

test_that("insert_macros works",{
  out = macroStan:::insert_macros(the_scaffold, hs_ordered)
  expect_true(all(names(out) %in% block_names))
  lengths = vapply(out, length, 1L)
  expect_true(all(lengths == 1))
})

test_that("blocks_to_stan_string works", {
  out = macroStan:::blocks_to_stan_string(
    macroStan:::insert_macros(the_scaffold, hs_ordered))
  validated = rstan::stanc(model_code = out)
  expect_true(validated$status)
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

test_that("verify_macro_list detects bad macros",{
  good_macro = structure(function(){}, class = c("stan_macro", "function"))
  bad_macro = function(){}
  expect_error(verify_macro_list(
    macro_files = list(hs = test_file),
    macro_list = list(bad = bad_macro), good = good_macro))
  expect_error(verify_macro_list(
    macro_files = list(hs = test_file),
    macro_list = list(good = good_macro), bad = bad_macro))
})
test_that("verify_macro_list checks names",{
  good_macro = structure(function(){}, class = c("stan_macro", "function"))
  expect_error(verify_macro_list(
    macro_list = list(good_macro), good = good_macro))
  expect_error(verify_macro_list(
    macro_list = list(good_macro), good_macro))
  expect_error(verify_macro_list(
    macro_list = list(good = good_macro), good= good_macro))
})
test_that("verify_macro_list returns list of macros",{
  good_macro = structure(function(){}, class = c("stan_macro", "function"))
  bad_macro = function(){}
  out = verify_macro_list(
    macro_files = list(hs = test_file),
    macro_list = list(good1 = good_macro), good2 = good_macro)
  expect_true(all(vapply(out, is_stan_macro, TRUE)))

})

# The big one
test_that("parse_stan_macros works on horseshoe", {
  tmp_out = tempfile(fileext = ".stan")
  out = macroStan:::parse_stan_macros(test_scaffold, tmp_out,
    macro_files = list(
    horseshoe = test_file), .validate_output = FALSE)
  expect_true(rstan::stanc(out)$status)
})

# Test with two macros
test_that("parse_stan_macros works on mv_ncp", {
  macro_files = list(
    ncp_mv = "../../inst/macros/ncp_mv.stan",
    ncp_mv_linpred = "../../inst/macros/ncp_mv_linpred.stan")
  scaffold = "../../inst/stan_scaffolds/ncp_mv.stan"

  tst_out = tempfile(fileext = ".stan")
  out = parse_stan_macros(scaffold, tst_out, macro_files = macro_files, .validate_output = FALSE)
  expect_equal(tst_out, out)
  expect_true(rstan::stanc(out)$status)
  # file.edit(tst_out)
  })

# Test with three macros
test_that("parse_stan_macros works on mv_ncp and hs", {
  macro_files = list(
    horseshoe = "../../inst/macros/horseshoe.stan",
    ncp_mv = "../../inst/macros/ncp_mv.stan",
    ncp_mv_linpred = "../../inst/macros/ncp_mv_linpred.stan")
  scaffold = "../../inst/stan_scaffolds/ncp_mv_hs.stan"

  tst_out = tempfile(fileext = ".stan")
  out = parse_stan_macros(scaffold, tst_out, macro_files = macro_files, .validate_output = FALSE)
  expect_equal(tst_out, out)
  expect_true(rstan::stanc(out)$status)
})
