context("Testing pr_invdistance()")

# IBE graph as undirected network
playnet <- delete_edges(IBE121, E(IBE121)[question != "play"] ) %>%
  as.undirected()
# plot(playnet, vertex.label.cex=0.7)


test_that("it works silently", {
  expect_silent(
    r <- pr_invdistance(playnet)
  )
})







context("Testing pr_sharedp()")

test_that("it works silently", {
  expect_silent(
    r <- pr_sharedp(playnet)
  )
})






context("Testing pairwise_redundancy()")

test_that("it works silently", {
  expect_silent(
    r <- pairwise_redundancy(playnet)
  )
  table(r$pr_redundancy)
  r %>%
    dplyr::filter(ego == "1024")
})



