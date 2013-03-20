\name{moody}
\alias{moody}
\title{Odds ratio within-group tie existence}
\usage{
  moody(g, vattr)
}
\arguments{
  \item{g}{object of class "igraph", the network}

  \item{vattr}{character, name of the vertex attribute}
}
\value{
  Numeric value of the measure.
}
\description{
  Odds ratio for connected, as opposed to disconnected,
  dyads depedning whether it is between or within-group,
  i.e. how much more likely the dyad will be connected if
  it is within-group.
}
\details{
  The measure takes values, like all odds ratios, from (0;
  Inf).
}
\examples{
data(Wnet)
moody(Wnet, "gender")
}
\references{
  Moody, Jim (2001) "Race, school integration, and
  friendship segregation in America", American Journal of
  Sociology, 107(3):679--377
}
\seealso{
  Other segregation.measures: \code{\link{assort}},
  \code{\link{assort.default}},
  \code{\link{assort.igraph}},
  \code{\link{assort.mixingm}}, \code{\link{coleman}},
  \code{\link{coleman.igraph}},
  \code{\link{coleman.mixingm}}, \code{\link{ei}},
  \code{\link{ei.igraph}}, \code{\link{ei.mixingm}},
  \code{\link{freeman}}, \code{\link{freeman.igraph}},
  \code{\link{freeman.mixingm}}, \code{\link{gamix}},
  \code{\link{smi}}, \code{\link{ssi}}
}

