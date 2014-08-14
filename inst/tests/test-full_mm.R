context("Sanity checks for 'full_mm'")


test_that("Given 3d mixing matrix the function returns the same", {
          mm <- array( 1:8, dim=c(3,3,2))
          expect_identical( mm, full_mm(mm) )
} )
