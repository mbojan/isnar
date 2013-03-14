\name{ei}
\alias{ei}
\title{Krackhard and Stern's E-I index}
\usage{
  ei(g, vattr, ...)
}
\arguments{
  \item{g}{object of class "igraph", the network}

  \item{vattr}{character scalar or vector of length equal
  to the size of \code{g}, vertex attribute for which
  mixing matrix is to be computed}

  \item{...}{other arguments, currently ignored}
}
\value{
  Numerical value of the E-I index for network \code{g}.
}
\description{
  An index proposed by Krackhard and Stern (1988) to
  capture relative prevalence of between- and within-group
  ties. From that perspective it can be interpreted as a
  measure of network segregation.
}
\examples{
ei(Wnet, "gender")
}
\seealso{
  Other segregation.measures: \code{\link{assort}},
  \code{\link{assort.default}},
  \code{\link{assort.igraph}}, \code{\link{assort.table}},
  \code{\link{coleman}}, \code{\link{coleman.igraph}},
  \code{\link{freeman}}, \code{\link{gamix}},
  \code{\link{moody}}, \code{\link{smi}}, \code{\link{ssi}}
}

