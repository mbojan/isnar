context("Creating mixing matrices")

test_that("creating 2d undirected mm from matrix sets proper attributes",
          {
            # undirected network (based on Wnet)
            x <- matrix(c(6, 0, 9, 7), 2, 2)
            m <- mixingm(x, directed=FALSE, foldit=FALSE, gsizes=c(5,5))
            expect_equal(as.numeric(x), as.numeric(m))
            expect_false(attr(m, "directed"))
            expect_equal(attr(m, "size"), 10)
            expect_equal(attr(m, "gsizes"), c(5,5))
          } )

test_that("folding 2d matrix for undirected network works",
          {
            x <- matrix(c(6, 4, 5, 7), 2, 2)
            m <- mixingm(x, directed=FALSE, foldit=TRUE, gsizes=c(5,5))
            xfolded <- matrix(c(6, 0, 9, 7), 2, 2)
            expect_equal(as.numeric(xfolded), as.numeric(m))
          } )

test_that("folding 3d matrix for undirected networks works",
          {
            # unfolded contact- and non-contact layers
            cl <- matrix(c(6, 4, 5, 7), 2, 2)
            ncl <- matrix(c(4, 10, 6, 3), 2, 2)
            a <- array( c(ncl, cl), dim=c(2,2,2))
            m <- mixingm(a, directed=FALSE, foldit=TRUE, gsizes=c(5,5))
            # expected result
            er <- array(c(4, 0, 16, 3, 6, 0, 9, 7), dim=c(2,2,2))
            expect_equal( as.numeric(m), as.numeric(er))
          } )

