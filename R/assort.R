#' Assortativity Coefficient
#'
#' Assortativity coefficient is a measure of segregation for social networks
#' due to Mark Newman (2002).
#'
#' The measure evaluates the relative prevalence of within-group ties. It is
#' based on the contact layer of the mixing matrix.
#'
#' Takes the value of 1 if all ties are within-group. The minimum can be
#' negative and depends on the distribution of ties in the network.
#'
#' @param g object of class "igraph", the network
#'
#' @param ... other arguments to/from other methods, currently ignored
#'
#' @return Numeric value of the index.
#' @export
#'
#' @references
#' Newman, M. J. and Girvan, M. (2002) "Mixing patterns and community structure
#' in networks", arXiv:cond-mat/0210146v1
#'
#' Newman, M. J. (2003) "Mixing patterns in networks" arXiv:cond-mat/0209450v2
#'
#' @example examples/assort.R
#' @family segregation measures
assort <- function(g, ...) UseMethod("assort")



#' @details Method for tables
#'
#' @method assort table
#' @export
#' @rdname assort
assort.table <- function(g, ...)
{
    if( length(dim(g)) == 3 )
      m <- g[,,2]
    else m <- g
    m <- symmetrize(m, "div")
    p <- m / sum(m)
    s <- sum( p %*% p ) # || e^2 ||
    rval <- ( sum(diag(p)) - s ) / (1 - s)
    rval
}

#' @details Method for igraph
#'
#' @param vattr character, name of the vertex attribute for which the measure
#' is to be calculated
#'
#' @method assort igraph
#' @export
#' @rdname assort
assort.igraph <- function(g, vattr, ...)
{
    # missing matrix
    g <- mixingm(g, vattr)
    NextMethod("assort")
}

#' Default method
#'
#' @method assort default
#' @export
#' @rdname assort
assort.default <- function(g, ...)
{
  g <- as.table(g)
  assort.table(g, ...)
}
