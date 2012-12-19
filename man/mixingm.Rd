\name{mixingm}
\alias{mixingm}
\title{Calculate network mixing matrix}
\usage{
  mixingm(g, vattr, full = FALSE, loops = any(is.loop(g)),
    ...)
}
\arguments{
  \item{g}{object of class "igraph", network to be
  analyzed}

  \item{vattr}{character scalar or vector of length equal
  to the size of \code{g}, vertex attribute for which
  mixing matrix is to be computed}

  \item{full}{logical, should a three-dimensional mixing
  array be returned instead of only the contact layer}

  \item{loops}{logical, are loops (self edges) admissible
  in \code{g}, defaults to the presence of loops in
  \code{g}}

  \item{...}{other arguments passed to/from other methods,
  currently passed to \code{\link{fold}}}
}
\value{
  If \code{full} is \code{FALSE}, the default, a
  two-dimensional square table network ties
  cross-classified. If it is \code{TRUE} a three
  dimensional array with dimensions "ego", "alter", and
  "tie".

  For undirected network the matrix is folded onto the
  upper triangle.
}
\description{
  Given a network and vertex attribute calculate two or
  three-dimensional mixing matrix.
}
\details{
  Mixing matrix is, traditionaly, a two-dimensional
  cross-classification of network ties depeding on the
  values of given vertex attribute of tie sender and tie
  receiver. A "generalized" mixing matrix is a
  three-dimensional array which cross-classifies \emph{all}
  network dyads depending on the values of the attribute
  for tie sender and tie reciever, and whether the dyad is
  connected or not. The two-dimensional version is a layer,
  so-called contact layer, of the three-dimensional
  version.  Be default, when \code{full} is \code{FALSE}
  the two-dimesional version is computed.

  If \code{vattr} is a character scalar it is interpreted
  as a name of the vertex attribute in \code{g}. Otherwise
  it can be a vector of length equal to \code{vcount(g)}
  containing the attribute.

  If the \code{full} is \code{TRUE} then \code{loops}
  determines whether to take loops into account when
  calculating the number of dyads in \code{g}.
}
\examples{
# some directed network
net <- graph(c(1,2, 1,3, 2,3,  4,5,  1,4, 1,5, 4,2, 5,3))
V(net)$type <- c(1,1,1, 2,2)

# plot it
plot(net, vertex.color=V(net)$type + 1, vertex.label.family="",
  layout=layout.fruchterman.reingold)

mixingm(net, "type")
mixingm(net, "type", full=TRUE)

# as undirected
mixingm( as.undirected(net), "type")
mixingm(net, "type", full=TRUE)
}

