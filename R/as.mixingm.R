#' Coerce to mixing matrix
#'
#' Functions for creating mixing matrices from other objects.
#'
#' Consult \code{\link{mixingm}} for more information on mixing matrices.
#'
#' @param object R object, see Details for available methods
#'
#' @param ... other arguments passed to/from other methods
#'
#' @return
#' An object of S3 class "mixingm" extending class "table".
#'
#' There are additional attributes storing extra information about the network:
#' network \code{size}, \code{gsizes}, and whether the network is
#' \code{directed}.
#'
#' @seealso \code{\link{mixingm}}
#'
#' @example examples/as.mixingm.R
#'
#' @export
as.mixingm <- function(object, ...) UseMethod("as.mixingm")




#============================================================================ 
# Method for tables


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
#' @param full logical, whether to return full 3-dimensional mixing matrix, see Details
#' @param gsizes numerical vector of group sizes
#' @param directed logical, whether the network is directed
#' @param loops logical, whether the network contains loops (self-ties)
#' @param size numeric network size, computed from group sizes by default
#'
#' @method as.mixingm table
#' @rdname as.mixingm
#' @export
as.mixingm.table <- function(object, full=FALSE, gsizes=NULL, directed=TRUE,
                             loops=FALSE, size=NULL, ...)
{
  ndim <- length(dim(object))
  stopifnot(ndim %in% 2:3)
  if(ndim == 3)
  {
    # calculate groups sizes
    as_mm_table3d(object, directed=directed, loops=loops)
  } else
  {
    as_mm_table2d(object, full=full, gsizes=gsizes,  size=size,
                  directed=directed, loops=loops)
  }
}

# three dimensional table
as_mm_table3d <- function(object, directed, loops)
{
  # get ego-alter margin
  eamargin <- apply(object, 1:2, sum)
  # compute group sizes
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
  mixingm(object, directed=directed, size=sum(gsizes), gsizes=gsizes)
}



# two-dimensional table
as_mm_table2d <- function(object, gsizes=NULL, size=NULL, full=FALSE,
                          directed=TRUE, loops=FALSE)
{
  # if 'full', reconstruct non-contact layer
  if(full)
  {
    # we need group sizes
    stopifnot(!is.null(gsizes))
    if(is.null(size)) size <- sum(gsizes)
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
  mixingm(rval, directed=directed, gsizes=gsizes, size=size)
}




#============================================================================ 
# Method for igraphs



#' @details In the method for "igraph" objects:
#' \itemize{
#' \item Be default, when \code{full} is \code{FALSE} the two-dimesional
#' version is computed.
#'
#' \item If \code{vattr} is a character scalar it is interpreted as a name of
#' the vertex attribute in graph \code{object}. Otherwise it can be a vector of
#' length equal to \code{vcount(object)} containing the attribute.
#'
#' \item If the \code{full} is \code{TRUE} then \code{loops} determines whether
#' to take loops into account when calculating the number of dyads in the graph \code{object}.
#' }
#'
#' @param vattr  character scalar or vector of length equal to the number of
#' nodes in the graph, i.e., \code{vcount(object)}. Vertex attribute for which
#' mixing matrix is to be computed.
#'
#' @method as.mixingm igraph
#' @rdname as.mixingm
#' @export
as.mixingm.igraph <- function(object, vattr, full=FALSE, loops=any(is.loop(object)), ...)
{
    # get attribute
    if(is.character(vattr) & length(vattr) == 1)
    {
        a <- igraph::get.vertex.attribute(object, vattr)
    } else
    {
        stopifnot( length(vattr) == vcount(object) )
        a <- vattr
    }
    # contact layer based on edgelist
    el <- igraph::get.edgelist(object, names=FALSE)
    u <- sort(unique(a))
    ego <- factor(a[ el[,1] ], levels=u)
    alter <- factor(a[ el[,2] ], levels=u)
    con <- table(ego=ego, alter=alter)
    # fold for undirected
    if(!igraph::is.directed(object))
        con <- fold(con, ...)
    # return contact layer if not full
    conmix <- mixingm(con,
              directed=is.directed(object),
              gsizes=table(a, dnn=NULL),
              size=vcount(object)
              )
    if(!full)
    {
      return(conmix)
    } else
    {
      as.mixingm( conmix,
                 directed=attr(conmix, "directed"),
                 gsizes=attr(conmix, "gsizes"),
                 size=attr(conmix, "size"),
                 loops=loops,
                 full=TRUE)
    }
}




#============================================================================ 
# Default method


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
