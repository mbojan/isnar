library(isnar)

context("Testing SMI")


test_that("SMI works", {
          g <- delete.edges(IBE121, E(IBE121)[ question != "play" ])
          smi(g, "female")
} )
