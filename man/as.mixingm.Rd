\name{as.mixingm}
\alias{as.mixingm}
\alias{as.mixingm.table}
\title{Coerce to mixing matrix}
\usage{
  as.mixingm(object, ...)

  \method{as.mixingm}{table} (object, full = FALSE,
    gsizes = NULL, directed = TRUE, loops = FALSE,
    size = ifelse(is.null(gsizes), NULL, sum(gsizes)))
}
\arguments{
  \item{object}{R object}

  \item{...}{other arguments passed to/from other methods}

  \item{full}{}

  \item{gsizes}{}

  \item{directed}{}

  \item{loops}{}

  \item{size}{}
}
\value{
  An object of S3 class "mixingm" extending class "table".
  If \code{full} is \code{FALSE}, the default, a
  two-dimensional square table with cross-classification of
  network ties. If \code{full} is \code{TRUE}, a three
  dimensional table with dimensions "ego", "alter", and
  "tie".

  For undirected network the matrix is folded onto the
  upper triangle (entries in lower triangle are 0).

  There are additional attributes storing extra information
  about the network: network \code{size},
  \code{group.sizes}, and whether the network is
  \code{directed}.
}
\description{
  Functions checking if object is a mixing matrix, or
  coerce it if possible.
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
  so-called "contact layer" of the three-dimensional mixing
  matrix.

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
}

