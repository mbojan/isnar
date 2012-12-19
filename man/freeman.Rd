\name{freeman}
\alias{freeman}
\title{Generalized Freeman's segregation index}
\usage{
  freeman(g, vattr, dis = NULL, more = FALSE)
}
\arguments{
  \item{g}{object of class "igraph" representing the
  network}

  \item{vattr}{name of vertex attribute in \code{g}}

  \item{dis}{numeric, optional true distribution of types,
  see Details}

  \item{more}{logical, should some more output be returned}
}
\value{
  The value of the Freeman's index.

  If \code{more} is \code{TRUE} some intermediate results
  returned in a list.
}
\description{
  Calculate generalized Freeman's segregation index for
  undirected netoworks with arbitrary number of groups.
}
\details{
  Freeman's segregation index (Freeman, 1978) is designed
  to capture the extent to which the defined types of
  vertices tend to have more edges with vertices from the
  same type than with other types (segregation).  Formally,
  the index compares the number of edges in the specified
  graph that connect vertices of different type to the
  number of ``inter-type'' edges that would be expected
  under pure random graph. High values indicate that there
  one would expect much more inter-type edges if they are
  to be created without considering the type of vertices,
  suggesting high segregation. Low values indicate that
  lower segregation. Especially, the index is 0 for the
  full graph (all possible edges are present).

  The index has some discontinuity as there are graph and
  type configurations that are characterized by the higher
  number of inter-type edges that is expected under a
  random graph. The index is truncates these situations and
  takes a value of 0.

  The original Freeman's formulation was for only two types
  of vertices.  Here it is extended to the arbitrary number
  of types. The modification only affects the way in which
  the expected number of inter-type edges under pure random
  graph is calculated.

  About the function itself:

  The function internally calculates the frequency of types
  of vertices in the supplied attributer \code{vattr}.
  However, it is possible to override this by specifying
  ``true'' type distribution with the \code{dis} argument.
  It is assumed to be a table (as returned by \code{table})
  or a numeric vector with frequencies of types of
  vertices. This may be especially usefull when dealing
  with large graphs with larger number of isolates.
}
\examples{
## White's data from Freeman's article
data(Wnet)
# segregation level
freeman(Wnet, "gender")
# using 'more' argument
freeman(Wnet, "gender", more=TRUE)
}
\references{
  Freeman, Linton C. (1978) Segregation in Social Networks,
  \emph{Sociological Methods & Research}
  \bold{6}(4):411--429
}
\seealso{
  Other segregation.measures: \code{\link{assort}},
  \code{\link{assort.default}},
  \code{\link{assort.igraph}}, \code{\link{assort.table}},
  \code{\link{coleman}}, \code{\link{gamix}},
  \code{\link{moody}}
}
\keyword{graphs}

