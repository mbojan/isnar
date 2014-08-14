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
#' @param mat numeric square (say \eqn{n \times n}{n*n}) matrix or array with
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
#' @export
mixingm <- function(g, ...) UseMethod("mixingm")

#' @method mixingm igraph
#' @export
#' @rdname mixingm
mixingm.igraph <- function(g, rattr, cattr=rattr, full=FALSE,
                            directed = is.directed(g),
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
  if(full)
  {
    return(full_mm(con, gsizes=table(ra, ca), directed=directed, loops=loops))
  } else
  {
    return(con)
  }
}


# Does it look like a valid mm
valid_mm <- function(m, square=TRUE, verbose=FALSE)
  # square = should m be square
{
  rval <- NULL
  if( !is.array(m) )
  {
    rval <- "'m' is not an array"
    stop(rval)
  }
  dims <- dim(m)
  if( !(length(dims) %in% 2:3) )
    rval <- c(rval, paste0("'m' should have 2 or 3 dimensions, has ", length(dims)))
  if( length(dims) == 3 && dims[3] != 2 )
    rval <- c(rval, paste0("third dimension should have two values, has ", dims[3]))
  if(square)
  {
    if( dims[1] != dims[2] )
      rval <- c(rval, paste0("dimensions 1 and 2 should be equal, are ", dims[1],
                             " and ", dims[2]) )
  }
  if(is.null(rval)) return(TRUE)
  if(verbose) return(rval)
  else return(FALSE)
}
