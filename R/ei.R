#' Krackhard and Stern's E-I index
#'
#' An index proposed by Krackhard and Stern (1988) to capture relative
#' prevalence of between- and within-group ties. From that perspective it can
#' be interpreted as a measure of network segregation.
#'
#' @param g object of class "igraph", the network
#'
#' @param vattr  character scalar or vector of length equal to the size of
#' \code{g}, vertex attribute for which mixing matrix is to be computed
#'
#' @param ... other arguments, currently ignored
#'
#' @return Numerical value of the E-I index for network \code{g}.
#'
#' @example examples/ei.R
#' @export
#' @family segregation measures
ei <- function(g, vattr, ...)
{
  m <- mixingm(g, vattr=vattr)
  p <- m / sum(m)
  pinternal <- sum(diag(p))
  diag(p) <- 0
  pexternal <- sum(p)
  return( pexternal - pinternal )
}
