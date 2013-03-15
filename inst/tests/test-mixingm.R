context("Mixing matrices")

test_that("Created mixing matrix is of proper class", {
          m2 <- mixingm(Wnet, "gender")
          m3 <- mixingm(Wnet, "gender", full=TRUE)
          expect_equal( class(m2), c("mixingm", "table"))
          expect_equal( class(m3), c("mixingm", "table"))
} )




context("Created mixing matrices have proper attributes")

test_that("2d mixing matrix for White data has all attributes",
          {
            m <- mixingm(Wnet, "gender", full=FALSE)
            expect_equal( as.numeric(attr(m, "group.sizes")), c(5,5))
            expect_equal( attr(m, "size"), 10 )
            expect_equal( attr(m, "directed"), FALSE)
          } )
