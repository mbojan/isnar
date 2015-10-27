assort(Wnet, "gender")
assort(EFnet, "type")


if( require(igraph, quietly = TRUE) ) {
  # value of 'assort' for full networks of different sizes
  f <- function(n)
  {
    gfull <- graph.full(n, directed=FALSE)
    V(gfull)$type <- rep(1:2, length=vcount(gfull))
    assort(gfull, "type")
  }
  set.seed(1)
  x <- sort(sample(5:100, 25) * 2)
  y <- sapply(x, f)
  plot(x, y, type="o",
       xlab="Network size", ylab="Assortativity coefficient",
       main="Assortativity coef. for full networks of different sizes")
}
