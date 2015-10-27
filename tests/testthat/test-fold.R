context("Testing folding matrices")


test_that("Folding non-square matrix gives an error", {
          m <- matrix(1, 2, 3)
          expect_error( fold(m) )
} )



test_that("Folding to upper triangle works", {
          m <- matrix(1, 2, 2)
          r <- matrix( c(1,0,2,1), 2, 2)
          expect_equal( fold(m), r )
} )


test_that("Folding to lower triangle works", {
          m <- matrix(1, 2, 2)
          r <- matrix( c(1,2,0,1), 2, 2)
          expect_equal( fold(m, "lower"), r )
} )
