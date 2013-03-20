\name{gamix}
\alias{gamix}
\title{Gupta-Anderson-May measure of within-group mixing}
\usage{
  gamix(g, vattr, debug = FALSE)
}
\arguments{
  \item{g}{object of class "igraph", the network}

  \item{vattr}{character, name of vertex attribute}

  \item{debug}{logical, return some intermediate results as
  attributes to the returned value}
}
\value{
  Numerical value of the measure.
}
\description{
  Measure of within-group mixing in networks proposed in
  Gupta, Anderson and May (1989).
}
\details{
  The measure varies between \code{-1/vcount(g)} for
  dissassortative mixing and 1 for perfect within-group
  mixing. It takes a value of 0 for proportionate mixing.
}
\examples{
data(Wnet)
gamix(Wnet, "gender")

data(EFnet)
gamix(EFnet, "type")
}
\references{
  Gupta, S., Anderson, R., May, R. (1989) "Networks of
  sexual contacts: implications for the pattern of spread
  of HIV", AIDS 3:807--817
}
\seealso{
  Other segregation.measures: \code{\link{assort}},
  \code{\link{assort.default}},
  \code{\link{assort.igraph}},
  \code{\link{assort.mixingm}}, \code{\link{coleman}},
  \code{\link{coleman.igraph}},
  \code{\link{coleman.mixingm}}, \code{\link{ei}},
  \code{\link{freeman}}, \code{\link{moody}},
  \code{\link{smi}}, \code{\link{ssi}}
}

