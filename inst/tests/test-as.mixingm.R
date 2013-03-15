context("Coercing tables to mixing matrices with as.mixingm")

# test three dimensional table

test_that("3d mixing matrix for Wnet is properly reconstructed",
          {
            m <- mixingm(Wnet, "gender", full=TRUE)
            r <- as.mixingm(m[,,2], full=TRUE, directed=TRUE)
            test_true(all( m == r ))
} )

# test two dimensional table


# context("Coercing arrays and matrices to mixing matrices with as.mixingm")
# 2- and 3-dimensional arrays
