# odds ratio for tie existence

moody <- function(g, vattr)
{
    m <- mixingm(g, vattr, full=TRUE)
    z <- apply(m, 3, function(x) 
        c(between= sum(x) - sum(diag(x)), within=sum(diag(x))) )
    offdiag <- z[ col(z) != row(z) ]
    prod(diag(z)) / prod(offdiag)
}
\name{moody}
\alias{moody}
\title{Odds ratio within-group tie existence}
\description{
Odds ratio for connected, as opposed to disconnected, dyads depedning whether
it is between or within-group, i.e. how much more likely the dyad will be
connected if it is within-group.
}
\usage{
moody(g, vattr)
}
\arguments{
  \item{g}{object of class "igraph", the network}
  \item{vattr}{character, name of the vertex attribute}
}
\details{
The measure takes values, like all odds ratios, from (0; Inf).
}
\value{
Numeric value of the measure.
}
\references{
Used by Moody in:

Moody, Jim (2001) "Race, school integration, and friendship segregation in
America", American Journal of Sociology, 107(3):679--377
}
\seealso{
% TODO
Other measures of segregation:

\code{\link{mixingm}}
}
\examples{
data(Wnet)
moody(Wnet, "gender")
}
