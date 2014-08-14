#' Coleman's homophily index
#'
#' Colemans's homphily index for directed networks.
#'
#' @param object R object, see available methods
#'
#' @param ... other arguments
#'
#' @details
#' Coleman's homophily index computes homophily scores for each group
#' defined by a vertex attribute.
#'
#' @return Vector of numeric values of the index.
#'
#' @references
#' Coleman, J. (1958) "Relational analysis: The study of social organizations
#' with survey methods", Human Organization 17:28--36.
#'
#' @example examples/coleman.R
#' @export
#' @family segregation measures
coleman <- function(object, ...) UseMethod("coleman")







#' @details
#' \code{object} can be a mixing matrix as returned by \code{\link{mixingm}} or
#' \code{\link{as.mixingm}}.
#'
#' @method coleman mixingm
#' @export
#' @rdname coleman
coleman.mixingm <- function(object, gsizes=NULL, ...)
{
  # take contact layer
  if( length(dim(object)) != 2 )
    m <- object[,,2]
  else m <- object
  # take group sizes
  stopifnot(!is.null(attr(object, "gsizes")))
  gsizes <- attr(object, "gsizes")
  # group outdegrees
  degsums <- rowSums(m)
  # expected number of within-group ties for each group
  ewg <- degsums * (gsizes - 1) / (sum(gsizes) - 1)
  # rval
  r <- (diag(m) - ewg) / (degsums - ewg)
  i <- diag(m) <= ewg
  repl <- (diag(m) - ewg) / ewg
  r[i] <- repl[i]
  structure(as.numeric(r), names=names(gsizes))
}



#' @details
#' \code{object} can be of class "igraph"
#' @param vattr character, vertex attribute
#' @method coleman igraph
#' @export
#' @rdname coleman
coleman.igraph <- function(object, vattr, ...)
{
  stopifnot(is.directed(object))
  object <- as.mixingm(object, vattr, full=TRUE)
  coleman(object, ...)
}

