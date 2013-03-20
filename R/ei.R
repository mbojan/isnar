#' Krackhard and Stern's E-I index
#'
#' An index proposed by Krackhard and Stern (1988) to capture relative
#' prevalence of between- and within-group ties. From that perspective it can
#' be interpreted as a measure of network segregation.
#'
#' @param object R object, see Details for available methods
#'
#' @param ... other arguments passed to/from other methods
#'
#' @return Numerical value of the E-I index.
#'
#' @example examples/ei.R
#' @export
#' @family segregation measures
ei <- function(object, ...) UseMethod("ei")

#' @details
#' Method for mixing matrices
#' @method ei mixingm
#' @rdname ei
#' @export
ei.mixingm <- function(object, ...)
{
  # extract contact layer
  if(length(dim(object)) == 3)
    m <- object[,,2]
  else m <- object
  p <- m / sum(m)
  pinternal <- sum(diag(p))
  diag(p) <- 0
  pexternal <- sum(p)
  return( pexternal - pinternal )
}

#' @details
#' Method for igraphs
#'
#' @param vattr character scalar or vector of length equal to the size of
#' \code{object}, vertex attribute for which mixing matrix is to be computed
#'
#' @method ei igraph
#' @rdname ei
#' @export
ei.igraph <- function(object, vattr, ...)
{
  m <- as.mixingm(object, vattr=vattr)
  ei(m, ...)
}
