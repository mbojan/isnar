if( require(igraph, quietly = TRUE)) {
  coleman(as.directed(Wnet, "mutual"), "gender")
  coleman(as.directed(EFnet, "mutual"), "type")
}
