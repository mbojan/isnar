if( require(igraph, quietly = TRUE) ) {
  data(Wnet)
  set.seed(2992)
  plot(Wnet, layout=layout.fruchterman.reingold,
       vertex.color=V(Wnet)$gender + 1,
       vertex.label=V(Wnet)$name, vertex.label.family="",
       main="White's (1975) data on kinship networks")
  legend("topleft", col=2:3, legend=c("Woman", "Man"), pch=19)
}
