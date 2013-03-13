\name{smi}
\alias{smi}
\title{Segregation Matrix Index}
\usage{
  smi(g, vattr, normalize = TRUE)
}
\arguments{
  \item{g}{object of class "igraph", the network}

  \item{vattr}{character, name of the node attribute
  designating groups}

  \item{normalize}{logical, whether normalized values
  should be returned, defaults to \code{TRUE}}
}
\value{
  Numeric vector of length equal to the number of groups in
  \code{g} according to \code{vattr} with the values of SMI
  for the groups.
}
\description{
  Segregation Matrix Index due to Freshtman (1997). A
  measure of network segregation.  Currently (and
  originally) supports only two groups.
}
\details{
  The Segregation Matrix Index (SMI) is calculated for
  every group separately. It compares the density within
  group to the density of between group ties of nodes
  belonging to that group.

  Non-normalized version is the ration of the within-group
  density to the between-group denisty, so varie between 0
  and infinity. The normalized version varies between 0 and
  1.
}
\examples{
data(Wnet)
smi( as.directed(Wnet, "mutual"), "gender")
}
\references{
  Freshtman, M. (1997) "Cohesive Group Segregation
  Detection in a Social Network by the Segregation Matrix
  Index", Social Networks, 19:193--207
}
\seealso{
  Other segregation.measures: \code{\link{assort}},
  \code{\link{assort.default}},
  \code{\link{assort.igraph}}, \code{\link{assort.table}},
  \code{\link{coleman}}, \code{\link{ei}},
  \code{\link{freeman}}, \code{\link{gamix}},
  \code{\link{moody}}, \code{\link{ssi}}
}

