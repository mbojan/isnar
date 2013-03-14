context("Mixing matrices")

test_that("Created mixing matrix is of proper class", {
          m2 <- mixingm(Wnet, "gender")
          m3 <- mixingm(Wnet, "gender", full=TRUE)
          expect_equal( class(m2), c("mixingm", "table"))
          expect_equal( class(m3), c("mixingm", "table"))
} )

