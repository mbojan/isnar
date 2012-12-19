context("Freeman segregation index")

test_that("Freeman index is 0 for full network", {
          library(igraph)
  g <- graph.full(10, directed=FALSE)
  V(g)$type <- rep(1:2, length=vcount(g))
  expect_that( freeman(g, "type"), equals(0))
} )
