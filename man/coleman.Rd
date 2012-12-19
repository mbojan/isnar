\name{coleman}
\alias{coleman}
\title{Coleman's index of network segregation}
\usage{
  coleman(g, vattr)
}
\arguments{
  \item{g}{object of class "igraph", the network}

  \item{vattr}{character, vertex attribute}
}
\value{
  Numeric value of the measure.
}
\description{
  Colemans's measure for segregation in networks. Designed
  in principle for directed networks, but no formal
  obstacles for using it in undirected context.
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
  Other measures of segregation.  \code{\link{mixingm}}
}

