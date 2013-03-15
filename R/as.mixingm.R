#' Coerce to mixing matrix
#'
#' Functions checking if object is a mixing matrix, or coerce it if possible.
#'
#' Mixing matrix is, traditionaly, a two-dimensional cross-classification of
#' network ties depeding on the values of given vertex attribute of tie sender
#' and tie receiver. A full mixing matrix is a three-dimensional array which
#' cross-classifies \emph{all} network \emph{dyads} depending on the values of
#' the attribute for tie sender and tie reciever, and whether the dyad is
#' connected or not. The two-dimensional version is a so-called "contact layer"
#' of the three-dimensional mixing matrix.
#'
#' @param object R object
#'
#' @param ... other arguments passed to/from other methods
#'
#' @return
#' An object of S3 class "mixingm" extending class "table".  If \code{full}
#' is \code{FALSE}, the default, a two-dimensional square table with
#' cross-classification of network ties. If \code{full} is \code{TRUE}, a three
#' dimensional table with dimensions "ego", "alter", and "tie".
#'
#' For undirected network the matrix is folded onto the upper triangle (entries
#' in lower triangle are 0).
#'
#' There are additional attributes storing extra information about the network:
#' network \code{size}, \code{group.sizes}, and whether the network is
#' \code{directed}.
#'
#' @export
as.mixingm <- function(object, ...) UseMethod("as.mixingm")


#' @details
#' If \code{object} is of class "table" it is expected to be either a
#' two-dimensional contact layer of the mixing matrix or a three-dimensional
#' array with both contact and non-contact layers (as the third dimension).
#'
#' For three-dimensional tables the function does the following things:
#' \enumerate{
#' \item Compute the group sizes.
#' \item Set attributes for network size, group sizes, and directed/undirected
#' characte of the network.
#' }
#'
#' For two-dimensional tables and argument \code{full} is TRUE the function:
#' \enumerate{
#' \item Reconstructs the non-contact layer of the mixing matrix based on group
#' sizes provided as an argument \code{gsizes}.
#' \item Set attributes for network size, group sizes, and directed/undirected
#' characte of the network.
#' }
#'
#' For two-dimensional tables and argument \code{full} is FALSE the function
#' sets the attributes for network size, group sizes, and directed/undirected
#' characte of the network based on supplied arguments.
#'
#' @param full logical, whether to return full 3-dimensional mixing matrix
#' @param gsizes numerical vector of group sizes
#' @param directed logical, whether the network is directed
#' @param loops logical, whether the network contains loops (self-ties)
#' @param size numeric network size, computed from group sizes by default
#'
#' @method as.mixingm table
#' @rdname as.mixingm
#' @export
as.mixingm.table <- function(object, full=FALSE, gsizes=NULL, directed=TRUE,
                             loops=FALSE,
                             size=ifelse(is.null(gsizes), NULL, sum(gsizes)) )
{
  ndim <- length(dim(object))
  stopifnot(ndim %in% 2:3)
  if(ndim == 3)
  {
    as_mm_table3d(object, directed=directed, loops=loops)
  } else
  {
    as_mm_table2d(object, full=full, gsizes=gsizes,  size=size,
                  directed=directed, loops=loops)
  }
}

# three dimensional table
as_mm_table3d <- function(object, directed=TRUE, loops=FALSE)
{
  eamargin <- apply(object, 1:2, sum)
  if(directed)
  {
    if(loops)
    {
      gsizes <- sqrt(diag(eamargin))
    } else
    {
      gsizes <- 0.5 * (1 + sqrt(1 + 4*diag(eamargin)))
    }
  } else
  {
    gsizes <- 0.5 * (1 + sqrt(1 + 8*diag(eamargin)))
  }
  structure(object, directed=directed, size=sum(gsizes), group.sizes=gsizes,
            class=c("mixingm", "table"))
}



# two-dimensional table
as_mm_table2d <- function(object, gsizes=NULL,
                      size=ifelse(is.null(gsizes), NULL, sum(gsizes)),
                      full=FALSE, directed=TRUE, loops=FALSE)
{
  if(full)
  {
    stopifnot(!is.null(gsizes))
    # compute ego-alter margin 'mar'
    o <- outer(gsizes, gsizes, "*")
    if(directed)
    {
        mar <- o
        if(!loops)
        {
            diag(mar) <- diag(o) - gsizes
        }
    } else {
            mar <- o
            mar[ lower.tri(mar) ] <- 0
        if(loops)
        {
            diag(mar) <- (diag(o) + gsizes) / 2
        } else {
            diag(mar) <- (diag(o) - gsizes) / 2
        }
    }
    # build the result
    rval <- array(NA, dim=c(dim(object), 2))
    rval[,,2] <- object
    rval[,,1] <- mar - object
    # set dimension names
    if(is.null(dimnames(object)))
    {
      dimnames(rval) <- list(NULL, NULL, tie=c(FALSE, TRUE))
    } else
    {
      dimnames(rval) <- c( dimnames(object)[1:2], list(tie=c(FALSE, TRUE)))
    }
  } else
  {
    rval <- object
  }
    structure(rval, size=size, group.sizes=gsizes, directed=directed,
              class=c("mixingm", "table"))
}


# @details
# The default S3 method tries to coerce \code{object} to a table and call the
# table method.
#
# @export
# @rdname as.mixingm
# @method as.mixingm default
as.mixingm.default <- function(object, ...)
{
  as.mixingm( as.table(object), ...)
}
