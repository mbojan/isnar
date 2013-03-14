#' Calculate network mixing matrix
#'
#' Given a network and vertex attribute calculate two or three-dimensional
#' mixing matrix.
#'
#'
#' Mixing matrix is, traditionaly, a two-dimensional cross-classification of
#' network ties depeding on the values of given vertex attribute of tie sender
#' and tie receiver. A "generalized" mixing matrix is a three-dimensional array
#' which cross-classifies \emph{all} network dyads depending on the values of
#' the attribute for tie sender and tie reciever, and whether the dyad is
#' connected or not. The two-dimensional version is a layer, so-called contact
#' layer, of the three-dimensional version.  Be default, when \code{full} is
#' \code{FALSE} the two-dimesional version is computed.
#' 
#' If \code{vattr} is a character scalar it is interpreted as a name of the
#' vertex attribute in \code{g}. Otherwise it can be a vector of length equal
#' to \code{vcount(g)} containing the attribute.
#' 
#' If the \code{full} is \code{TRUE} then \code{loops} determines whether to
#' take loops into account when calculating the number of dyads in \code{g}.
#'
#' @param g,x object of class "igraph", network to be analyzed
#'
#' @param vattr  character scalar or vector of length equal to the size of
#' \code{g}, vertex attribute for which mixing matrix is to be computed
#'
#' @param full logical, should a three-dimensional mixing array be returned
#' instead of only the contact layer
#'
#' @param loops logical, are loops (self edges) admissible in \code{g},
#' defaults to the presence of loops in \code{g}
#'
#' @param ... other arguments passed to/from other methods, currently passed to
#' \code{\link{fold}}
#'
#' @return
#' An object of class "mixingm" extending class "table" (S3).  If \code{full}
#' is \code{FALSE}, the default, a two-dimensional square table with
#' cross-classification of network ties. If \code{full} is \code{TRUE}, a three
#' dimensional table with dimensions "ego", "alter", and "tie".
#'
#' For undirected network the matrix is folded onto the upper triangle (entries
#' in lower triangle are 0).
#'
#' @example examples/mixingm.R
#' @export
mixingm <- function(g, vattr, full=FALSE, loops=any(is.loop(g)), ...)
{
    # get attribute
    if(is.character(vattr) & length(vattr) == 1)
    {
        a <- igraph::get.vertex.attribute(g, vattr)
    } else
    {
        stopifnot( length(vattr) == vcount(g) )
        a <- vattr
    }
    # contact layer based on edgelist
    el <- igraph::get.edgelist(g, names=FALSE)
    u <- sort(unique(a))
    ego <- factor(a[ el[,1] ], levels=u)
    alter <- factor(a[ el[,2] ], levels=u)
    con <- table(ego=ego, alter=alter)
    # fold for undirected
    if(!igraph::is.directed(g))
        con <- fold(con, ...)
    # return contact layer if not full
    if(!full)  return(con)
    # ego-alter margin
    sums <- table(a)
    o <- outer(sums, sums, "*")
    if(igraph::is.directed(g))
    {
        mar <- o
        if(!loops)
        {
            diag(mar) <- diag(o) - sums
        }
    } else {
            mar <- o
            mar[ lower.tri(mar) ] <- 0
        if(loops)
        {
            diag(mar) <- (diag(o) + sums) / 2
        } else {
            diag(mar) <- (diag(o) - sums) / 2
        }
    }
    # build the result
    rval <- array(NA, dim=c(length(u), length(u), 2))
    dimnames(rval) <- list(ego=u, alter=u, tie=c(FALSE, TRUE))
    rval[,,2] <- con
    rval[,,1] <- mar - con
    attr(rval, "size") <- vcount(g)
    attr(rval, "group.sizes") <- table(a, dnn=NULL)
    class(rval) <- c("mixingm", "table")
    rval
}

#' Printing method
#'
#' @method print mixingm
#' @export
#' @rdname mixingm
print.mixingm <- function(x, ...)
{
  z <- structure(as.numeric(x), dim=dim(x), dimnames=dimnames(x))
  cat("Mixing matrix\n")
  cat("Network size:", attr(x, "size"), "\n")
  tab <- attr(x, "group.sizes")
  names(dimnames(tab)) <- "Groups and their sizes"
  print(tab)
  print(z)
}





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
