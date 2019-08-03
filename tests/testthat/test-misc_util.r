test_that("first works", {
  expect_equal(first((1:5) > 3), 4)
  expect_equal(first((1:5) > 5), integer(0))
})

test_that("sort_by works", {
  # mpg_expec = mtcars %>% arrange(disp) %>% head %>% pull(mpg)
  mpg_expec = c(33.9, 30.4, 32.4, 27.3, 30.4, 22.8)
  expect_equal(head(sort_by(mtcars, disp))$mpg, mpg_expec)
  # wt_expec = mtcars %>% arrange(desc(disp)) %>% head %>% pull(wt)
  wt_expec = c(5.250, 5.424, 5.345, 3.845, 3.440, 3.570)
  expect_equal(head(sort_by(mtcars, disp, .desc = TRUE))$wt, wt_expec)

  # with some expressions
  # drat_expec = mtcars %>% arrange(cyl*100 + hp) %>% head %>% pull(drat)
  drat_expec = c(4.93, 3.69, 4.22, 4.08, 4.08, 4.43)
  expect_equal(head(sort_by(mtcars, cyl * 100 + hp))$drat, drat_expec)
})
test_that("filter_rows works",{
  # filter(mtcars, cyl==6, disp < 200)
  expect_equal(nrow(filter_rows(mtcars, cyl == 4)), 11)
  expect_equal(unique(filter_rows(mtcars, cyl == 4)$cyl), 4)
  expect_equal(nrow(filter_rows(mtcars,
                    cyl == 6 & disp < 200)), 5)
  expect_equal(filter_rows(mtcars, cyl == 9), mtcars[integer(0),])
})
