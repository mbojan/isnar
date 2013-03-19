#' Network mixing matrix
#'
#' Creating network mixing matrices.
#'
#'
#' Mixing matrix is, traditionaly, a two-dimensional cross-classification of
#' network ties depeding on the values of given vertex attribute of tie sender
#' (or "ego") and tie receiver (or "alter"). A full mixing matrix is a
#' three-dimensional array that cross-classifies \emph{all} network
#' \emph{dyads} depending on the values of the attribute for tie sender, tie
#' reciever, and whether the dyad is connected or not. The two-dimensional
#' version is a so-called "contact layer" of the three-dimensional version.
#'
#'
#' @param x numeric square (say \eqn{n*n}{n \times n} matrix or array with
#' \code{dim=c(n, n, 2)}
#'
#' @param gsizes numerical vector of group sizes
#' @param directed logical, whether the network is directed
#' @param loops logical, whether the network contains loops (self-ties)
#' @param size numeric network size, computed from group sizes by default
#' @param foldit logical, whether mixingm matrix for undirected networks should
#' be folded
#'
#' @return
#' An object of S3 class "mixingm" extending class "table".
#'
#' For undirected network and if \code{foldit} is TRUE (default), the matrix is
#' folded onto the upper triangle (entries in lower triangle are 0).
#'
#' @example examples/mixingm.R
#' @export
mixingm <- function(x, gsizes=NULL, directed=TRUE, loops=FALSE, size=NULL,
                    foldit=TRUE)
{
  stopifnot(is.array(x))
  # check for proper dimensionality of 'x'
  dims <- dim(x)
  stopifnot( length(dims) %in% 2:3 )
  stopifnot( dims[1] == dims[2] )
  if(length(dims) == 3) stopifnot(dims[3] == 2)
  d3 <- length(dims) == 3
  # other arguments
  stopifnot( length(gsizes) == dims[1] )
  if(!is.null(gsizes)) size <- sum(gsizes)
  # TODO check if 'x' can be mixing matrix
  # If undirected, fold onto upper triangle
  if( !directed && foldit )
  {
    if(d3)
    {
      mat <- apply(x, 3, fold, direction="upper")
    } else
    {
      mat <- fold(x, direction="upper")
    }
  }
  r <- as.table(mat)
  structure( r,
            dimnames=dimnames(r),
            directed=directed,
            loops=loops,
            gsizes=gsizes,
            size=size,
            class=c("mixingm", "table")
            )
}


#' @param x object of class "mixingm"
#'
#' @method print mixingm
#' @export
#' @rdname mixingm
print.mixingm <- function(x, ...)
{
  z <- structure(as.numeric(x), dim=dim(x), dimnames=dimnames(x))
  cat("Mixing matrix\n")
  cat("Is directed:", attr(x, "directed"), "\n")
  cat("Network size:", attr(x, "size"), "\n")
  tab <- attr(x, "gsizes")
  cat("Group sizes:\n")
  print(tab)
  print(z)
}











#============================================================================ 
# Mixing matrix, ver2
# Creating mixing matrix given two vertex attributes
mixingm2 <- function(g, ...) UseMethod("mixingm2")

mixingm2.igraph <- function(g, rattr, cattr=rattr, full=FALSE,
                            loops=any(is.loop(g)), ...)
{
  # get attributes
  if( is.character(rattr) && length(rattr)==1 )
  {
    ra <- igraph::get.vertex.attribute(g, rattr)
  } else
  {
    stopifnot( length(rattr) == vcount(g))
    ra <- rattr
  }
  if( is.character(cattr) && length(cattr)==1 )
  {
    ca <- igraph::get.vertex.attribute(g, cattr)
  } else
  {
    stopifnot( length(cattr) == vcount(g))
    ca <- cattr
  }
  # compute contact layer based on edge list
  el <- igraph::get.edgelist(g, names=FALSE)
  ego <- factor( ra[ el[,1] ], levels=sort(unique(ra)))
  alter <- factor(ca[ el[,2] ], levels=sort(unique(ca)))
  con <- table(ego=ego, alter=alter)
  con
}
