\name{coleman}
\alias{coleman}
\alias{coleman.igraph}
\title{Coleman's index of network segregation}
\usage{
  coleman(object, ...)

  \method{coleman}{igraph} (object, vattr)
}
\arguments{
  \item{object,}{R object, see available methods}

  \item{vattr}{character, vertex attribute}
}
\value{
  Numeric values of the measure.
}
\description{
  Colemans's measure for segregation in directed networks.
}
\details{
  Method for "igraph"
}
\examples{
data(Wnet)
coleman(Wnet, "gender")

data(EFnet)
coleman(EFnet, "type")
}
\references{
  Coleman, J. (1958) "Relational analysis: The study of
  social organizations with survey methods", Human
  Organization 17:28--36.
}
\seealso{
  Other segregation.measures: \code{\link{assort}},
  \code{\link{assort.default}},
  \code{\link{assort.igraph}}, \code{\link{assort.table}},
  \code{\link{ei}}, \code{\link{freeman}},
  \code{\link{gamix}}, \code{\link{moody}},
  \code{\link{smi}}, \code{\link{ssi}}
}

