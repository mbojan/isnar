context("Coercing tables to mixing matrices with as.mixingm")

test_that("reconstructing 3d mm from 2d matrix for undirected network works",
          {
            # undirected network (based on Wnet)
            m <- matrix(c(6, 0, 9, 7), 2, 2)
            er <- array(c(4, 0, 16, 3, 6, 0, 9, 7), dim=c(2,2,2))
            r <- as.mixingm(m, gsizes=c(5,5), directed=FALSE, full=TRUE)
            expect_true(all(r == er))
          } )

test_that("reconstructing 3d mm from 2d matrix for directed network works",
          {
            # directed network (based on Wnet)
            m <- matrix(c(6, 0, 9, 7), 2, 2)
            er <- array(c(14, 25, 16, 13, 6, 0, 9, 7), dim=c(2,2,2))
            r <- as.mixingm(m, gsizes=c(5,5), directed=TRUE, full=TRUE)
            expect_true(all(r == er))
          } )

test_that("coercing Catania data to 2d mixing matrix gives correct result",
          {
            m <- as.mixingm(Catania)
            expect_true( inherits(m, "mixingm") )
            expect_identical( as.numeric(Catania), as.numeric(m))
            expect_true( attr(m, "directed") )
            expect_true( is.null(attr(m, "size")))
            expect_true( is.null(attr(m, "gsizes")))
          } )









context("Coercing igraphs to mixingm with as.mixingm")

test_that("coercing Wnet to 2d mixingm",
            {
              m <- as.mixingm(Wnet, "gender")
              # numbers
              expect_equal( as.numeric(m), c(6, 0, 9, 7))
              expect_equal( as.numeric(attr(m, "gsizes")), c(5, 5))
              expect_equal( attr(m, "size"), 10)
            } )

test_that("coercing Wnet to 3d mixingm works",
          {
            m <- as.mixingm(Wnet, "gender", full=TRUE)
            # numbers
            expect_equal( as.numeric(m), c(4, 0, 16, 3, 6, 9, 7) )
            expect_equal( as.numeric(attr(m, "gsizes")), c(5,5))
            expect_equal( attr(m, "size"), 10 )
          } )












context("Coercing other objects to mixingm with as.mixingm (default method)")

# TODO finish
test_that("coercing a matrix works",
          {
            mat <- matrix( c(6, 0, 9, 7), 2, 2)
            as.mixingm(mat, directed=FALSE)
          } )


test_that("Created mixing matrix is of proper class", {
          m2 <- mixingm(Wnet, "gender")
          m3 <- mixingm(Wnet, "gender", full=TRUE)
          expect_equal( class(m2), c("mixingm", "table"))
          expect_equal( class(m3), c("mixingm", "table"))
} )
