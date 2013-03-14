\name{mixingm}
\alias{mixingm}
\alias{mixingm.igraph}
\alias{print.mixingm}
\title{Network mixing matrix}
\usage{
  mixingm(object, ...)

  \method{mixingm}{igraph} (object, vattr, full = FALSE,
    loops = any(is.loop(object)), ...)

  \method{print}{mixingm} (x, ...)
}
\arguments{
  \item{object}{R object}

  \item{...}{other arguments passed to/from other methods}

  \item{vattr}{character scalar or vector of length equal
  to the size of \code{g}, vertex attribute for which
  mixing matrix is to be computed}

  \item{full}{logical, should a three-dimensional mixing
  array be returned instead of only the contact layer}

  \item{loops}{logical, are loops (self edges) admissible
  in \code{g}, defaults to the presence of loops in
  \code{g}}

  \item{x}{object of class "mixingm"}
}
\value{
  An object of class "mixingm" extending class "table"
  (S3).  If \code{full} is \code{FALSE}, the default, a
  two-dimensional square table with cross-classification of
  network ties. If \code{full} is \code{TRUE}, a three
  dimensional table with dimensions "ego", "alter", and
  "tie".

  For undirected network the matrix is folded onto the
  upper triangle (entries in lower triangle are 0).
}
\description{
  Creating network mixing matrices.
}
\details{
  Mixing matrix is, traditionaly, a two-dimensional
  cross-classification of network ties depeding on the
  values of given vertex attribute of tie sender and tie
  receiver. A full mixing matrix is a three-dimensional
  array which cross-classifies \emph{all} network
  \emph{dyads} depending on the values of the attribute for
  tie sender and tie reciever, and whether the dyad is
  connected or not. The two-dimensional version is a
  so-called "contact layer" of the three-dimensional
  version.

  In the method for "igraph" objects: \itemize{ \item Be
  default, when \code{full} is \code{FALSE} the
  two-dimesional version is computed.

  \item If \code{vattr} is a character scalar it is
  interpreted as a name of the vertex attribute in
  \code{g}. Otherwise it can be a vector of length equal to
  \code{vcount(g)} containing the attribute.

  \item If the \code{full} is \code{TRUE} then \code{loops}
  determines whether to take loops into account when
  calculating the number of dyads in \code{g}. }
}
\examples{
# some directed network
net <- graph(c(1,2, 1,3, 2,3,  4,5,  1,4, 1,5, 4,2, 5,3))
V(net)$type <- c(1,1,1, 2,2)

mixingm(net, "type")
mixingm(net, "type", full=TRUE)

# as undirected
mixingm( as.undirected(net), "type")
mixingm(net, "type")
mixingm(net, "type", full=TRUE)
}

