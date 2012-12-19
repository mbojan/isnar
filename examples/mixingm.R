# some directed network
net <- graph(c(1,2, 1,3, 2,3,  4,5,  1,4, 1,5, 4,2, 5,3))
V(net)$type <- c(1,1,1, 2,2)

# plot it
plot(net, vertex.color=V(net)$type + 1, vertex.label.family="",
  layout=layout.fruchterman.reingold)

mixingm(net, "type")
mixingm(net, "type", full=TRUE)

# as undirected
mixingm( as.undirected(net), "type")
mixingm(net, "type", full=TRUE)
