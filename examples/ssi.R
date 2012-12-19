# artificial EF data
if( require(mbtools) )
{
data(EFnet)
x <- ssi(EFnet, "type")
x
# show it on picture
a <- V(EFnet)$type
k <- rescale(x, .9, .1)
plot( EFnet, layout=layout.fruchterman.reingold,
    vertex.color= gray(k),
    vertex.label.family="",
    vertex.shape=c("circle", "square")[a],
    vertex.label.color=gray( (1-k) > .4 ) )

# For White's kinship data
data(Wnet)
x <- ssi(Wnet, "gender")
x
# plot it
a <- V(Wnet)$gender
k <- rescale(x, .9, .5)
set.seed(1234)
plot( Wnet, layout=layout.fruchterman.reingold,
    main="Node segregation in White's kinship data",
    vertex.label.family="",
    vertex.label=V(Wnet)$name,
    vertex.color= gray(k),
    vertex.shape=c("circle", "csquare")[a],
    vertex.label.color="black")
legend( "topleft", legend=c("Men", "Women"), pch=c(0,1), col=1)
}
