context("Assortativity coefficient")

test_that("AC for Catania data is equal to the value from the paper", {
  data(Catania)
  v <- assort(Catania)
  tv <- 0.621 # value from Newman's paper
  tol <- 0.004
  expect_true( v >= tv - tol & v <= tv + tol)
} )
