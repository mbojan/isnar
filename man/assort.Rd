\name{assort}
\alias{assort}
\alias{assort.default}
\alias{assort.igraph}
\alias{assort.table}
\title{Assortativity Coefficient}
\usage{
  assort(g, ...)

  \method{assort}{table} (g, ...)

  \method{assort}{igraph} (g, vattr, ...)

  \method{assort}{default} (g, ...)
}
\arguments{
  \item{g}{object of class "igraph", the network}

  \item{...}{other arguments to/from other methods,
  currently ignored}

  \item{vattr}{character, name of the vertex attribute for
  which the measure is to be calculated}
}
\value{
  Numeric value of the index.
}
\description{
  Assortativity coefficient is a measure of segregation for
  social networks due to Mark Newman (2002).

  Default method
}
\details{
  The measure evaluates the relative prevalence of
  within-group ties. It is based on the contact layer of
  the mixing matrix.

  Takes the value of 1 if all ties are within-group. The
  minimum can be negative and depends on the distribution
  of ties in the network.

  Method for tables

  Method for igraph
}
\examples{
data(Wnet)
assort(Wnet, "gender")

data(EFnet)
assort(EFnet, "type")


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
\references{
  Newman, M. J. and Girvan, M. (2002) "Mixing patterns and
  community structure in networks",
  arXiv:cond-mat/0210146v1

  Newman, M. J. (2003) "Mixing patterns in networks"
  arXiv:cond-mat/0209450v2
}
\seealso{
  Other segregation.measures: \code{\link{coleman}},
  \code{\link{freeman}}
}

