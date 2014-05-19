
set.seed(2992)
plot(EFnet, layout=layout.fruchterman.reingold,
    vertex.color=V(EFnet)$type+1, vertex.label.family="",
    sub="Source: Echenique & Fryer (2006)",
    main="Neighborhood racial segregation\n in a fictional city" )
