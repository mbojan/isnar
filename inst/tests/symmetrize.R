context("Symmetrizing matrices")

test_that("Old tests", {
  m <- matrix(1:16, 4, 4)
  rvalDiv <- symmetrize(m, "div")
  rvalIntdiv <- symmetrize(m, "intdiv")

  expect_that( sum(m), equals(sum(rvalDiv)) )
  expect_that( sum(m), equals(sum(rvalIntdiv)) )
  expect_that( m + t(m), equals(rvalDiv + t(rvalDiv)) )
  expect_that( m + t(m), equals(rvalIntdiv + t(rvalIntdiv)) )
} )
