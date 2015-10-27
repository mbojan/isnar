if(require(igraph, quietly = TRUE)) {
  data(Wnet)
  smi( as.directed(Wnet, "mutual"), "gender")
}
