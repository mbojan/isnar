\name{mixingm}
\alias{mixingm}
\alias{print.mixingm}
\title{Network mixing matrix}
\usage{
  mixingm(x, gsizes = NULL, directed = TRUE, loops = FALSE,
    size = NULL, foldit = TRUE)

  \method{print}{mixingm} (x, ...)
}
\arguments{
  \item{x}{numeric square (say \eqn{n*n}{n \times n} matrix
  or array with \code{dim=c(n, n, 2)}}

  \item{gsizes}{numerical vector of group sizes}

  \item{directed}{logical, whether the network is directed}

  \item{loops}{logical, whether the network contains loops
  (self-ties)}

  \item{size}{numeric network size, computed from group
  sizes by default}

  \item{foldit}{logical, whether mixingm matrix for
  undirected networks should be folded}

  \item{x}{object of class "mixingm"}
}
\value{
  An object of S3 class "mixingm" extending class "table".

  For undirected network and if \code{foldit} is TRUE
  (default), the matrix is folded onto the upper triangle
  (entries in lower triangle are 0).
}
\description{
  Creating network mixing matrices.
}
\details{
  Mixing matrix is, traditionaly, a two-dimensional
  cross-classification of network ties depeding on the
  values of given vertex attribute of tie sender (or "ego")
  and tie receiver (or "alter"). A full mixing matrix is a
  three-dimensional array that cross-classifies \emph{all}
  network \emph{dyads} depending on the values of the
  attribute for tie sender, tie reciever, and whether the
  dyad is connected or not. The two-dimensional version is
  a so-called "contact layer" of the three-dimensional
  version.
}

