#' Segregation Matrix Index
#'
#' Segregation Matrix Index due to Freshtman (1997). A measure of network
#' segregation.  Currently (and originally) supports only two groups.
#'
#' The Segregation Matrix Index (SMI) is calculated for every group separately.
#' It compares the density within group to the density of between group ties of
#' nodes belonging to that group.
#'
#' Non-normalized version is the ration of the within-group density to the
#' between-group denisty, so varie between 0 and infinity. The normalized
#' version varies between 0 and 1.
#'
#' @param g object of class "igraph", the network
#'
#' @param vattr character, name of the node attribute designating groups
#'
#' @param normalize logical, whether normalized values should be returned,
#' defaults to \code{TRUE}
#'
#' @return
#' Numeric vector of length equal to the number of groups in \code{g} according
#' to \code{vattr} with the values of SMI for the groups.
#'
#' @references
#' Freshtman, M. (1997) "Cohesive Group Segregation Detection in a Social
#' Network by the Segregation Matrix Index", Social Networks, 19:193--207
#'
#' @family segregation measures
#' @export
#' @example examples/smi.R
smi <- function(g, vattr, normalize=TRUE)
{
  stopifnot(is.directed(g))
  # only for two groups for now
  if( length(unique(get.vertex.attribute(g, vattr))) > 2 )
      stop("currently 'smi' supports two groups")
  # mixing matrix
  mm <- mixingm(g=g, vattr=vattr, full=TRUE)
  pmm <- prop.table(mm, c(1,2))
  r <- diag( pmm[,,2] ) / pmm[1,2,2]
  if( normalize )
      return( (r - 1) / (r + 1) )
  else return(r)
}
