\name{gamix}
\alias{gamix}
\alias{gamix.igraph}
\alias{gamix.mixingm}
\title{Gupta-Anderson-May measure of within-group mixing}
\usage{
  gamix(object, ...)

  \method{gamix}{mixingm} (object, debug = FALSE, ...)

  \method{gamix}{igraph} (object, vattr, ...)
}
\arguments{
  \item{object}{R object, see Details for available
  methods}

  \item{...}{other objects passed to/from other methods}

  \item{debug}{logical, return some intermediate results as
  attributes to the returned value}

  \item{vattr}{character, name of vertex attribute}
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

  Method for mixing matrices

  Method for igraphs
}
\examples{
gamix(Wnet, "gender")
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
  \code{\link{ei.igraph}}, \code{\link{ei.mixingm}},
  \code{\link{freeman}}, \code{\link{freeman.igraph}},
  \code{\link{freeman.mixingm}}, \code{\link{orwg}},
  \code{\link{orwg.igraph}}, \code{\link{orwg.mixingm}},
  \code{\link{smi}}, \code{\link{smi.igraph}},
  \code{\link{smi.mixingm}}, \code{\link{ssi}}
}

