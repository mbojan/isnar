#' Assortativity Coefficient
#'
#' Assortativity coefficient is a measure of segregation for social networks
#' due to Mark Newman (2002).
#'
#' The measure evaluates the relative prevalence of within-group ties. It is
#' based on the contact layer of the mixing matrix.
#'
#' Assortativity coefficient is 1 if all ties are within-group.
#' The minimum can be negative, but not less than -1, and depends on the
#' relative number of ties of nodes in different groups. If the network
#' conforms to "proportionate mixing", the coefficient is 0.
#'
#' @param g R object, see available methods
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



#' @details If 'g' is a table it is treated as a mixing matrix. Two-dimensional
#' table is interpreted as a contact layer. Three-dimensional table is
#' interpreted as a full mixing matrix \eqn{m_{ghy}}{m[ijt]} cross-classyfying
#' all dyads, in which 'i' and 'j' correspond to group membership of ego and
#' alter respectively. Layers t=1 and t=2 are assumed to be contact and
#' noncontact layers respectively.
#'
#' @method assort mixingm
#' @export
#' @rdname assort
assort.mixingm <- function(g, ...)
{
  if( length(dim(g)) != 2 )
    m <- g[,,2]
  else m <- g
  m <- symmetrize(m, "div")
  p <- m / sum(m)
  s <- sum(colSums(p) * rowSums(p))
  rval <- ( sum(diag(p)) - s ) / (1 - s)
  rval
}

#' @details If \code{g} is an object of class "igraph" the measure is
#' calculated for the vertex attribute specified with \code{vattr}.
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
    NextMethod("assort", ...)
}

#' @details Any other objects passed as \code{g} are coerced to a table and the
#' table method is called.
#'
#' @method assort default
#' @export
#' @rdname assort
assort.default <- function(g, ...)
{
  m <- as.mixingm(g)
  assort.mixingm(m, ...)
}
