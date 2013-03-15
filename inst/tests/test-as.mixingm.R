context("Coercing tables to mixing matrices with as.mixingm")

test_that("reconstructing 3d mm from 2d matrix undirected network",
          {
            # undirected network (based on Wnet)
            m <- matrix(c(6, 0, 9, 7), 2, 2)
            er <- array(c(4, 0, 16, 3, 6, 0, 9, 7), dim=c(2,2,2))
            r <- as.mixingm(m, gsizes=c(5,5), directed=FALSE, full=TRUE)
            expect_true(all(r == er))
          } )

test_that("reconstructing 3d mm from 2d matrix directed network",
          {
            # directed network (based on Wnet)
            m <- matrix(c(6, 0, 9, 7), 2, 2)
            er <- array(c(14, 25, 16, 13, 6, 0, 9, 7), dim=c(2,2,2))
            r <- as.mixingm(m, gsizes=c(5,5), directed=TRUE, full=TRUE)
            expect_true(all(r == er))
          } )


# TODO
# using as.matrix on
# 1. mixingm objects
# 2. igraph networks
# 3. other objects
