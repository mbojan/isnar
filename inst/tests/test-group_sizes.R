context("Proper group sizes are computed")


test_that("Group sizes for White network are correct", {
          mm <- mixingm2( Wnet, "gender", full=TRUE )
          gs <- group_sizes(mm, directed=FALSE, loops=FALSE )
          expect_equivalent( gs, c(5, 5))
} )
