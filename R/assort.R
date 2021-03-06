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
#' @param object R object, see available methods
#'
#' @param ... other arguments to/from other methods
#'
#' @return Numeric value of the index.
#'
#' @seealso 
#' Mixing matrices: \code{\link{mixingm}}
#'
#' @references
#' Newman, M. J. and Girvan, M. (2002) "Mixing patterns and community structure
#' in networks", arXiv:cond-mat/0210146v1
#'
#' Newman, M. J. (2003) "Mixing patterns in networks" arXiv:cond-mat/0209450v2
#'
#' @example examples/assort.R
#' @export
#' @family segregation measures
assort <- function(object, ...) UseMethod("assort")



#' @details
#' If \code{object} is a table it is treated as a mixing matrix.
#' Two-dimensional table is interpreted as a contact layer. Three-dimensional
#' table is interpreted as a full mixing matrix \eqn{m_{ghy}}{m[ghy]}
#' cross-classyfying all dyads, in which 'g' and 'h' correspond to group
#' membership of ego and alter respectively. Layers y=1 and y=2 are assumed to
#' be non-contact and contact layers respectively. In the 3-d case only
#' \code{g[,,2]} is used.
#'
#' @method assort table
#' @export
#' @rdname assort
assort.table <- function(object, ...)
{
  stopifnot( valid_mm(object) )
  if( length(dim(object)) == 3 )
  {
    m <- object[,,2]
  } else
  {
    m <- object
  }
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
assort.igraph <- function(object, vattr, ...)
{
  # missing matrix
  object <- mixingm(object, vattr)
  assort(object, ...)
}




#' @details For any other classes, object \code{g} are coerced to a table and the
#' table method is called.
#'
#' @method assort default
#' @export
#' @rdname assort
assort.default <- function(object, ...)
{
  m <- as.table(object)
  assort.table(m, ...)
}
