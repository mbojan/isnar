#' Coleman's index of network segregation
#'
#' Colemans's measure for segregation in directed networks.
#'
#' @param object, R object, see available methods
#'
#' @return Numeric values of the measure.
#'
#' @references
#' Coleman, J. (1958) "Relational analysis: The study of social organizations
#' with survey methods", Human Organization 17:28--36.
#'
#' @example examples/coleman.R
#' @export
#' @family segregation measures
coleman <- function(object, ...) UseMethod("coleman")

#' @details Method for "igraph"
#' @param vattr character, vertex attribute
#' @method coleman igraph
#' @export
#' @rdname coleman
coleman.igraph <- function(object, vattr)
{
  stopifnot(inherits(object, "igraph"))
  stopifnot(is.directed(object))
  m <- mixingm(object, vattr, full=TRUE)
  # group out-degrees
  degsums <- rowSums(m[,,2])
  # group sizes
  gsizes <- table(igraph::get.vertex.attribute(object, vattr))
  # expected number of within-group ties for each group
  ewg <- degsums * (gsizes - 1) / (vcount(object) - 1)
  # rval
  r <- (diag(m[,,2]) - ewg) / (degsums - ewg)
  i <- diag(m[,,2]) <= ewg
  repl <- (diag(m[,,2]) - ewg) / ewg
  r[i] <- repl[i]
  structure(as.numeric(r), names=names(gsizes))
}

# method for mixing matrices
# TODO probably not tables
coleman.table <- function(object, ...)
{
  if( length(dim(object)) != 2 )
    m <- object[,,2]
  else m <- object
}
