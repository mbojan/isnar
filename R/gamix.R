#' Gupta-Anderson-May measure of within-group mixing
#'
#' Measure of within-group mixing in networks proposed in Gupta, Anderson and
#' May (1989).
#'
#' The measure varies between \code{-1/vcount(g)} for dissassortative mixing
#' and 1 for perfect within-group mixing. It takes a value of 0 for
#' proportionate mixing.
#'
#' @param g object of class "igraph", the network
#'
#' @param vattr character, name of vertex attribute
#'
#' @param debug logical, return some intermediate results as attributes to the returned value
#'
#' @return Numerical value of the measure.
#'
#' @references
#' Gupta, S., Anderson, R., May, R. (1989) "Networks of sexual contacts:
#' implications for the pattern of spread of HIV", AIDS 3:807--817
#'
#' @export
#' @family segregation measures
#' @example examples/gamix.R
gamix <- function (g, vattr, debug=FALSE)
{
    stopifnot(inherits(g, "igraph"))
    m <- symmetrize(mixingm(g, vattr))
    p <- sweep(m, 1, rowSums(m), "/")
    w <- eigen(p)$values
    rval <- (sum(w) - 1)/(dim(m)[1] - 1)
    if(debug)
	    structure(rval, mix=m, mixp=p, e=w)
    else
	    rval
}
