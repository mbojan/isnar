\name{ei}
\alias{ei}
\alias{ei.igraph}
\alias{ei.mixingm}
\title{Krackhard and Stern's E-I index}
\usage{
  ei(object, ...)

  \method{ei}{mixingm} (object, ...)

  \method{ei}{igraph} (object, vattr, ...)
}
\arguments{
  \item{object}{R object, see Details for available
  methods}

  \item{...}{other arguments passed to/from other methods}

  \item{vattr}{character scalar or vector of length equal
  to the size of \code{object}, vertex attribute for which
  mixing matrix is to be computed}
}
\value{
  Numerical value of the E-I index.
}
\description{
  An index proposed by Krackhard and Stern (1988) to
  capture relative prevalence of between- and within-group
  ties. From that perspective it can be interpreted as a
  measure of network segregation.
}
\details{
  Method for mixing matrices

  Method for igraphs
}
\examples{
ei(Wnet, "gender")
}
\seealso{
  Other segregation.measures: \code{\link{assort}},
  \code{\link{assort.default}},
  \code{\link{assort.igraph}},
  \code{\link{assort.mixingm}}, \code{\link{coleman}},
  \code{\link{coleman.igraph}},
  \code{\link{coleman.mixingm}}, \code{\link{freeman}},
  \code{\link{freeman.igraph}},
  \code{\link{freeman.mixingm}}, \code{\link{gamix}},
  \code{\link{moody}}, \code{\link{smi}}, \code{\link{ssi}}
}

