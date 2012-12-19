\name{EFnet}
\alias{EFnet}
\title{Example data from Echenique and Fryer (2006)}
\format{Object of class "igraph". An undirected network with vertex
attribute \code{type} (values 1 or 2).}
\source{
  Echenique, Federico and Roland G. Fryer, Jr. (2006) A
  Measure of Segregation Based On Social Interactions
}
\description{
  Artificial example data from Echenique & Fryer (2006)
  representing a city with black and white neighbourhoods.
}
\details{
  This data is taken from Echenique & Fryer (2006, figure
  III). The data represent a fictional city composed of 30
  neighborhoods that are either black or white.

  The data is a list of two components. The \code{network}
  component contains a network adjacency matrix that
  represents the grid structure of neighborhood
  proximities. For a given pair of two neighborhoods
  \code{i} and \code{j} if the entry \code{network[i,j]} is
  equal to 0 then they are not adjacent. Non-zero value
  mean that these two neighborhoods are adjacent. The
  non-zero entries are rescaled so that for every
  neighborhood they sum to 1.

  The second component \code{type} defines a type of the
  neighborhood, 1 is ``black'', 2 is ``white''.
}
\examples{
data(EFnet)

set.seed(2992)
plot(EFnet, layout=layout.fruchterman.reingold,
    vertex.color=V(EFnet)$type+1, vertex.label.family="",
    sub="Source: Echenique & Fryer (2006)",
    main="Neighborhood racial segregation\\n in a fictional city" )
}
\keyword{datasets}

