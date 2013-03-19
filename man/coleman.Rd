\name{coleman}
\alias{coleman}
\alias{coleman.igraph}
\title{Coleman's homophily index}
\usage{
  coleman(object, ...)

  \method{coleman}{igraph} (object, vattr, ...)
}
\arguments{
  \item{object}{R object, see available methods}

  \item{...}{other arguments}

  \item{vattr}{character, vertex attribute}
}
\value{
  Vector of numeric values of the index.
}
\description{
  Colemans's homphily index for directed networks.
}
\details{
  Coleman's homophily index computes homophily scores for
  each group defined by a vertex attribute.

  Method for "igraph"
}
\examples{
data(Wnet)
coleman(as.directed(Wnet, "mutual"), "gender")

data(EFnet)
coleman( as.directed(EFnet, "mutual"), "type")
}
\references{
  Coleman, J. (1958) "Relational analysis: The study of
  social organizations with survey methods", Human
  Organization 17:28--36.
}
\seealso{
  Other segregation.measures: \code{\link{assort}},
  \code{\link{assort.default}},
  \code{\link{assort.igraph}},
  \code{\link{assort.mixingm}}, \code{\link{ei}},
  \code{\link{freeman}}, \code{\link{gamix}},
  \code{\link{moody}}, \code{\link{smi}}, \code{\link{ssi}}
}

