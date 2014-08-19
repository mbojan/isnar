#' Create a 3-dimensional mixingm matrix
#'
#' Given contact layer of the mixing matrix compute a full 3-dimensional mixing
#' matrix by rebuilding the non-contact layer from supplied group sizes and
#' other arguments. 
#'
#' @param cl numeric matrix with contact layer of the mixing matrix
#' @param gsizes numeric vector or matrix with group sizes, see Details.
#' @param directed logical, whether the network is directed
#' @param loops logical, whether loops (self-ties) are allowed
#'
#' Contact layer of the mixing matrix is a cross-classification of ties
#' according to the attributes of tie sender (ego) and tie receiver (alter).
#' Classically, the same attribute is used for ego and alter resulting in a
#' square mixing matrix. In such cases \code{cl} shoud be square with \code{cl[i,j]} being
#' the number of ties from actors in group \code{i} to actors in group \code{j}
#' and \code{gsizes} should be a numeric vector with number of nodes in each group.
#' Consequently, it must hold that \code{length(gsizes) == nrow(cl) == ncol(cl)}.
#'
#' In more general case we can use different node attributes for ego and
#' alter. Then \code{cl} does not have to be square. In such cases
#' \code{gsizes} should be a cross-tabulation of nodes according to 
#' their values on both attributes. See Examples.
#'
#' @return
#' \code{full_mm} returns a full three-dimenstional mixing matrix as an array
#' with \code{dim} attribute equal to \code{c( nrow(cl), ncol(cl), 2 )}.
#'
#' @export
#'
#' @example examples/full_mm.R

full_mm <- function(cl, gsizes, directed=TRUE, loops=FALSE)
{
  # if 3d and dims ok, return cl
  if( length(dim(cl)) == 3 )
  {
    stopifnot(dim(cl)[3] == 2)
    return(cl)
  }
  ## Compute ego-alter margin
  gsizes <- as.table(gsizes)
  ndims <- length(dim(gsizes))
  stopifnot( ndims %in% 1:2 )
  if( ndims == 1)
  {
    gs <- gsizes
  } else
  {
    dtab <- as.data.frame(as.table(gsizes))
    gs <- dtab$Freq
  }
  o <- outer(gs, gs, "*")
  # Adjust 'o' according to 'loops' and 'directed'
  if(directed)
  {
    mar <- o
    if(!loops)
    {
        diag(mar) <- diag(o) - gs
    }
  } else {
    mar <- o
    mar[ lower.tri(mar) ] <- 0
    if(loops)
    {
        diag(mar) <- (diag(o) + gs) / 2
    } else {
        diag(mar) <- (diag(o) - gs) / 2
    }
  }
  # collapse if different groups
  if( ndims == 2 )
  {
    a1 <- apply(mar, 1, function(r) tapply(r, dtab[,1], sum))
    mar <- apply(a1, 1, function(k) tapply(k, dtab[,2], sum))
    mar <- t(mar)
  }
  rval <- array(NA, dim=c( dim(cl), 2 )) 
  rval[,,1] <- mar - cl
  rval[,,2] <- cl
  # set dimension names
  if(is.null(dimnames(cl)))
  {
    dimnames(rval) <- list(NULL, NULL, tie=c(FALSE, TRUE))
  } else
  {
    dimnames(rval) <- c( dimnames(cl)[1:2], list(tie=c(FALSE, TRUE)))
  }
  rval
}
