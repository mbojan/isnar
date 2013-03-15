context("Coleman homophily index")

test_that("Coleman index works for Hnatiuk data", {
          library(igraph)
          g <- graph( c(0,1, 1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,9, 9,0 ) + 1, directed=TRUE)
          V(g)$a <- c(1,1, 2,2, 3,3,3,3,3,3)
          coleman(g, "a")
} )

test_that("Coleman index gives correct results for his example data",
          {
            # coleman's matrix
            mat <- matrix(c(45, 20, 15, 20), 2, 2)
            # rebuild mixing matrix given group sizes
            mmat <- as.mixingm(mat, full=TRUE, directed=TRUE, gsizes=c(60, 40))
            r <- coleman(mmat)
            # NOTE In Colemans paper (1958) he uses approximation in computing
            # expected number of ties within group 'i'. The function is exact.
            # Original results were: boys=0.375 and girls=0.167
            expect_equal(r, c(0.38125, 0.175))
          } )
