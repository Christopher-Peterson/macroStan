test_that("remove_comments removes comments", {
  test_seq_template = "//int b = 2;
  int a = 1 /*{sq2}*/;
  // another comment?
  /* this is one line
  and another
  and a third */
  int x;"
  out = remove_comments(test_seq_template)
  # This just removes a bunch of the remaining white space
  out_cmp = trimws(gsub("(  )+", "", gsub("\n", "", out, fixed = TRUE)))
  expect_equal(out_cmp, "int a = 1;int x;")
  expect_equal(nchar(test_seq_template), nchar(out))
})

test_that("remove internal blocks nabs the blocks", {
  txt1 = "int x;
  int N;
  { int tmp;
  // do some stuff
  }"
  txt2 = "int {|x|};
  int N;
  { int tmp;
  // do some stuff
  }"
  expect_equal(remove_internal_blocks(txt1),
               "int x;\n  int N;\n  {")
  expect_equal(remove_internal_blocks(txt2),
               "int {|x|};\n  int N;\n  {")
})

test_that("find_block can find the right block",{
  stan_dmy = "functions {
  // these are functions
  }
  data{
  // these are data
  }"
  expect_equal(trimws(find_block(stan_dmy, "data")),
               "// these are data")
  expect_type(glue(stan_dmy, .open = "[[", .close = "]]"),
              "character")
})
test_that("post_declaration_position works with a variety of data types",{
  # These first ones are fully declarations,
  # so they should return nchar(x) + 1
  expect_post_pos = function(x) {
    expect_equal(post_declaration_position(x), nchar(x) + 1)
  }
  expect_post_pos("int<lower=0> x;")
  expect_post_pos("vector[12] x;")
  expect_post_pos("ordered_vector[13] x;")
  expect_post_pos("ordered_vector[13] x;
                  simplex[12] y;
                  matrix[1,2] z;
                  real<lower=0,upper=1> p[2];")
  expect_equal(post_declaration_position(NULL), 1)
  expect_equal(post_declaration_position("// just a comment\n"), 1)
})
test_that("post_declaration_position can tell where declarations end",{
  dec_txt = "ordered_vector[13] x;
   simplex[12] y;
   matrix[1,2] z;
   real<lower=0,upper=1> p[2];"
  txt = paste(dec_txt,
   "// post
   { int tmp;
   }", sep = "\n")
  expect_equal(post_declaration_position(txt),   nchar(dec_txt) + 1)
})

test_that("separate_declarations works",{
  dec_txt = "ordered_vector[13] x;
   simplex[12] y;
   matrix[1,2] z;
   real<lower=0,upper=1> p[2];"
  post_txt =  "
  // post
   { int tmp;
   }"
  txt = paste0(dec_txt,post_txt)
  expec = list(declarations = dec_txt,
               post = post_txt)
  expect_equal(separate_declarations(txt), expec)
  # Note: comments on last declaration line will get
  # improperly separated
  # This isn't a big deal, but should be fixed at some point.
})

test_that("get_blocks can separate a stan file", {
  expected = list(
    data = "
    int x;
    int y; // comment
    ", parameters = "
    real theta;
    ", `transformed parameters` =
    list(declarations = "
    real theta1;",post = "
    theta1 = theta + 1;
    ")
  )
  txt = glue_data(expected,
"data {{|data|}}
parameters {{|parameters|}}
transformed parameters{{|`transformed parameters`$declarations|}{|`transformed parameters`$post|}}",
.open = "{|", .close = "|}")
  expect_identical(get_blocks(txt), expected)
})
