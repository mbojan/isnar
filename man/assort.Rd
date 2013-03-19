\name{assort}
\alias{assort}
\alias{assort.default}
\alias{assort.igraph}
\alias{assort.mixingm}
\title{Assortativity Coefficient}
\usage{
  assort(g, ...)

  \method{assort}{mixingm} (g, ...)

  \method{assort}{igraph} (g, vattr, ...)

  \method{assort}{default} (g, ...)
}
\arguments{
  \item{g}{R object, see available methods}

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
}
\details{
  The measure evaluates the relative prevalence of
  within-group ties. It is based on the contact layer of
  the mixing matrix.

  Assortativity coefficient is 1 if all ties are
  within-group. The minimum can be negative, but not less
  than -1, and depends on the relative number of ties of
  nodes in different groups. If the network conforms to
  "proportionate mixing", the coefficient is 0.

  If 'g' is a table it is treated as a mixing matrix.
  Two-dimensional table is interpreted as a contact layer.
  Three-dimensional table is interpreted as a full mixing
  matrix \eqn{m_{ghy}}{m[ijt]} cross-classyfying all dyads,
  in which 'i' and 'j' correspond to group membership of
  ego and alter respectively. Layers t=1 and t=2 are
  assumed to be contact and noncontact layers respectively.

  If \code{g} is an object of class "igraph" the measure is
  calculated for the vertex attribute specified with
  \code{vattr}.

  Any other objects passed as \code{g} are coerced to a
  table and the table method is called.
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
  \code{\link{coleman.igraph}}, \code{\link{ei}},
  \code{\link{freeman}}, \code{\link{gamix}},
  \code{\link{moody}}, \code{\link{smi}}, \code{\link{ssi}}
}

