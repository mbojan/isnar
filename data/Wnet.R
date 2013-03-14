
e <- structure(list(V1 = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 4, 
5, 5, 6, 6, 6, 7, 7, 8, 8), V2 = c(2, 4, 5, 6, 7, 9, 10, 4, 5, 
6, 10, 5, 6, 6, 7, 7, 9, 10, 9, 10, 9, 10)), .Names = c("V1", 
"V2"), row.names = c(NA, -22L), class = "data.frame")

v <- structure(list(intergraph_id = 1:10, name = c("Mother", "Sister", 
"Brother's Wife", "Sister's Daughter", "Brother's Daughter", 
"Father", "Brother", "Sister's Husband", "Brother's Son", "Sister's Son"
), gender = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L)), .Names = c("intergraph_id", 
"name", "gender"), row.names = c(NA, -10L), class = "data.frame")

library(igraph)
Wnet <- graph.data.frame(e, directed=FALSE, vert=v)
rm(e, v)
