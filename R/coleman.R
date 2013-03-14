#' Coleman's index of network segregation
#'
#' Colemans's measure for segregation in directed networks.
#'
#' @param g object of class "igraph", the network
#'
#' @param vattr character, vertex attribute
#'
#' @return Numeric value of the measure.
#'
#' @references
#' Coleman, J. (1958) "Relational analysis: The study of social organizations
#' with survey methods", Human Organization 17:28--36.
#'
#' @example examples/coleman.R
#' @export
#' @family segregation measures
coleman <- function(g, vattr)
{
  stopifnot(inherits(g, "igraph"))
  stopifnot(is.directed(g))
  a <- get.vertex.attribute(g, vattr)
  m <- mixingm(g, vattr)
  # expected number of WG choices
  # NOTE should the degree be "out" for the directed networks?
  if( is.directed(g) )
  {
    degsums <- tapply( degree(g, mode="out"), a, sum )
  } else {
    degsums <- tapply( degree(g, mode="all"), a, sum )
  }
  gsize <- table(a)
  ewg <- degsums * (gsize - 1) / (vcount(g) - 1)
  # rval
  r <- (diag(m) - ewg) / (degsums - ewg)
  i <- diag(m) <= ewg
  repl <- (diag(m) - ewg) / ewg
  r[i] <- repl[i]
  structure(as.numeric(r), names=names(gsize))
}
