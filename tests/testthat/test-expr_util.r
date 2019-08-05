test_that("is_assignment works", {
  expr_list = rlang::parse_exprs(c(
    "x = foo(bar)", "x",
    "x == y",
    "y <- foo(bar)",
    "y ~ foo(bar)",
    "x",
    "x + 7"))
  expect_true(is_assignment(expr_list[[1]]))
  expect_false(is_assignment(expr_list[[2]]))
  expect_false(is_assignment(expr_list[[3]]))
  expect_true(is_assignment(expr_list[[4]]))
  expect_false(is_assignment(expr_list[[5]]))
  expect_false(is_assignment(expr_list[[6]]))
  expect_false(is_assignment(expr_list[[7]]))
  expect_error(is_assignment())
  expect_error(is_assignment(`x = 1`))
  expect_error(is_assignment(1))
  expect_error(is_assignment("x = 1"))
  })
test_that("assignment_lhs returns reasonable values", {
  expr_list = rlang::parse_exprs(c(
    "x = foo(bar)", "x",
    "y <- foo(bar)", "y",
    "y ~ foo(bar)"))
  expect_equal(assignment_lhs(expr_list[[1]]), expr_list[[2]])
  expect_equal(assignment_lhs(expr_list[[3]]), expr_list[[4]])
  expect_equal(assignment_lhs(expr_list[[5]]), expr_list[[5]])
})
test_that("assignment_rhs returns reasonable values", {
  expr_list = rlang::parse_exprs(c(
    "x = foo(bar)", "foo(bar)",
    "y <- foo(bar)",
    "y ~ foo(bar)"))
  expect_equal(assignment_rhs(expr_list[[1]]), expr_list[[2]])
  expect_equal(assignment_rhs(expr_list[[3]]), expr_list[[2]])
  expect_true(rlang::is_missing(assignment_rhs(expr_list[[4]])))
})
test_that("args_as_char works", {
  nquote_call = rlang::parse_expr(
    "x = foo(a, b = 1, c = 2, d = f)")
  quote_call = rlang::parse_expr(
    'x = foo("a", b = "1", c = "2", d = "f")')
  expect_equal(args_as_char(nquote_call), quote_call)
  expect_equal(args_as_char(quote_call), quote_call)
 # once again, without an assignment
  nquote_call2 = rlang::parse_expr(
    "foo(a, b = 1, c = 2, d = f)")
  quote_call2 = rlang::parse_expr(
    'foo("a", b = "1", c = "2", d = "f")')
  expect_equal(args_as_char(nquote_call2), quote_call2)
  expect_equal(args_as_char(quote_call2), quote_call2)
})
test_that("parse_assignment works", {
  expect_identical(parse_assignment("abc = {{1, 2}, {3, 4}}"),
                   rlang::parse_expr('abc = "{{1, 2}, {3, 4}}"'))
  expect_identical(macroStan:::parse_assignment("abc", "rhs"),"abc")
  expect_identical(macroStan:::parse_assignment("abc", "lhs"),
                   rlang::sym("abc"))
  expect_identical(parse_assignment("x = y"),
                   rlang::parse_expr('x = "y"'))
  expect_identical(parse_assignment("x = [y,1]"),
                   rlang::parse_expr('x = "[y,1]"'))
  expect_identical(parse_assignment("[x,1]", "rhs"),
                   rlang::parse_expr('"[x,1]"'))
  expect_identical(parse_assignment("x", "rhs"),
                   rlang::parse_expr('"x"'))
  expect_identical(parse_assignment("x", "lhs"),
                   rlang::parse_expr("x"))
})

