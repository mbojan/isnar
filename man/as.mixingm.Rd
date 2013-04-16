\name{as.mixingm}
\alias{as.mixingm}
\alias{as.mixingm.igraph}
\alias{as.mixingm.table}
\title{Coerce to mixing matrix}
\usage{
  as.mixingm(object, ...)

  \method{as.mixingm}{table} (object, full = FALSE,
    gsizes = NULL, directed = TRUE, loops = FALSE,
    size = NULL, ...)

  \method{as.mixingm}{igraph} (object, vattr, full = FALSE,
    loops = any(igraph::is.loop(object)), ...)
}
\arguments{
  \item{object}{R object, see Details for available
  methods}

  \item{...}{other arguments passed to/from other methods}

  \item{full}{logical, whether to return full 3-dimensional
  mixing matrix, see Details}

  \item{gsizes}{numerical vector of group sizes}

  \item{directed}{logical, whether the network is directed}

  \item{loops}{logical, whether the network contains loops
  (self-ties)}

  \item{size}{numeric network size, computed from group
  sizes by default}

  \item{vattr}{character scalar or vector of length equal
  to the number of nodes in the graph, i.e.,
  \code{vcount(object)}. Vertex attribute for which mixing
  matrix is to be computed.}
}
\value{
  An object of S3 class "mixingm" extending class "table".

  There are additional attributes storing extra information
  about the network: network \code{size}, \code{gsizes},
  and whether the network is \code{directed}.
}
\description{
  Functions for creating mixing matrices from other
  objects.
}
\details{
  Consult \code{\link{mixingm}} for more information on
  mixing matrices.

  If \code{object} is of class "table" it is expected to be
  either a two-dimensional contact layer of the mixing
  matrix or a three-dimensional array with both contact and
  non-contact layers (as the third dimension).

  For three-dimensional tables the function does the
  following things: \enumerate{ \item Compute the group
  sizes. \item Set attributes for network size, group
  sizes, and directed/undirected characte of the network. }

  For two-dimensional tables and argument \code{full} is
  TRUE the function: \enumerate{ \item Reconstructs the
  non-contact layer of the mixing matrix based on group
  sizes provided as an argument \code{gsizes}. \item Set
  attributes for network size, group sizes, and
  directed/undirected characte of the network. }

  For two-dimensional tables and argument \code{full} is
  FALSE the function sets the attributes for network size,
  group sizes, and directed/undirected characte of the
  network based on supplied arguments.

  In the method for "igraph" objects: \itemize{ \item Be
  default, when \code{full} is \code{FALSE} the
  two-dimesional version is computed.

  \item If \code{vattr} is a character scalar it is
  interpreted as a name of the vertex attribute in graph
  \code{object}. Otherwise it can be a vector of length
  equal to \code{vcount(object)} containing the attribute.

  \item If the \code{full} is \code{TRUE} then \code{loops}
  determines whether to take loops into account when
  calculating the number of dyads in the graph
  \code{object}. }
}
\examples{
# some directed network
net <- graph(c(1,2, 1,3, 2,3,  4,5,  1,4, 1,5, 4,2, 5,3))
V(net)$type <- c(1,1,1, 2,2)

as.mixingm(net, "type")
as.mixingm(net, "type", full=TRUE)

# as undirected
as.mixingm( as.undirected(net), "type")
as.mixingm(net, "type")
as.mixingm(net, "type", full=TRUE)
}
\seealso{
  \code{\link{mixingm}}
}

