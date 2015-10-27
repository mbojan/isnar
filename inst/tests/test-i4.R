context("Testing issue #4")

set.seed(123)
g <- sample_gnp(12, p=0.3)
V(g)$k3 <- rep(3:1, each=4)
isnar::mixingm(g, "k3", full=TRUE)
m <- isnar::mixingm(g, "k3", full=TRUE)


test_that("all entries non-negative", {
  expect_true( all( m >= 0 ) )
} )

