#' Coleman's index of network segregation
#'
#' Colemans's measure for segregation in networks. Designed in principle for
#' directed networks, but no formal obstacles for using it in undirected
#' context.
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
#' @seealso Other measures of segregation.  \code{\link{mixingm}}
#'
#' @example examples/coleman.R
#' @export
coleman <- function(g, vattr)
{
    stopifnot(inherits(g, "igraph"))
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
    if( is.directed(g) )
    {
        r <- (diag(m) - ewg) / (degsums - ewg)
        i <- diag(m) <= ewg
        repl <- (diag(m) - ewg) / ewg
        r[i] <- repl[i]
    } else
    {
        warning("coleman's index for undirected networks is experimental")
        r <- (diag(m)*2 - ewg) / (degsums - ewg)
        i <- diag(m)*2 <= ewg 
        repl <- (diag(m)*2 - ewg) / ewg
        r[i] <- repl[i]
    }
    structure(as.numeric(r), names=names(gsize))
}
