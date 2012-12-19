context("Coleman homophily index")

test_that("Coleman index works for Hnatiuk data", {
          library(igraph)
          g <- graph( c(0,1, 1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,9, 9,0 ) + 1, directed=TRUE)
          V(g)$a <- c(1,1, 2,2, 3,3,3,3,3,3)
          coleman(g, "a")
} )
