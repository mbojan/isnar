#' Generalized Freeman's segregation index
#'
#' Calculate Freeman's segregation index for undirected netoworks with
#' arbitrary number of groups.
#'
#' Freeman's segregation index (Freeman, 1978) is designed to capture the
#' extent to which the defined groups of vertices tend to have more edges with
#' vertices from the same group than with other groups.  Formally, the index
#' compares the observed number of between-group ties with the number of
#' between-group ties that would be expected if ties would be created randomly.
#'
#' The index has some discontinuity as there are network and group configurations
#' that are characterized by the higher number of between-group ties that is
#' expected under a random graph. The index is truncates these situations and
#' takes a value of 0.
#'
#' The original Freeman's formulation was for only two types of vertices.  Here
#' it is extended to the arbitrary number of types. The modification only
#' affects the way in which the expected number of inter-type edges under pure
#' random graph is calculated.
#'
#' The function internally calculates the frequency of types of vertices in the
#' supplied attributer \code{vattr}. However, it is possible to override this
#' by specifying ``true'' type distribution with the \code{dis} argument. It is
#' assumed to be a table (as returned by \code{table}) or a numeric vector with
#' frequencies of types of vertices. This may be especially usefull when
#' dealing with large graphs with larger number of isolates.
#'
#' @param object R object, see Details for available methods
#'
#' @param ... other arguments passed to/from other methods
#'
#' @return 
#' The value of the Freeman's index.
#'
#' If \code{more} is \code{TRUE}, some intermediate results are returned in a
#' list.
#'
#' @references
#' Freeman, Linton C. (1978) Segregation in Social Networks, \emph{Sociological
#' Methods & Research} \bold{6}(4):411--429
#'
#' @example examples/freeman.R
#' @keywords graphs
#' @export
#' @family segregation measures
freeman <- function(object, ...) UseMethod("freeman")


#' @details
#' Method for mixing matrices
#'
#' @param dis numeric, optional true distribution of types, see Details
#'
#' @param more logical, should some more output be returned
#'
#' @method freeman mixingm
#' @export
#' @rdname freeman
freeman.mixingm <- function(object, dis=attr(object, "gsizes"), more=FALSE, ...)
{
  stopifnot(!attr(object, "directed"))
  # need group sizes
  stopifnot(!is.null(attr(object, "gsizes")))
  # take contact layer
  if( length(dim(object)) == 3 )
    mat <- object[,,2]
  else mat <- object
  # number of cross-group ties
  cct <- sum(mat) - sum(diag(mat))
  # group distribution
  btab <- dis
  n <- sum(mat)
  m <- sum(btab)
  ecct <- (n * (sum(btab)^2 - sum(btab^2))) / (m * (m - 1))
  s <- ecct - cct
  if(s < 0)  s <- 0
  if(more)
      return(list(ecct = ecct, cct = cct, n = n, m = m, btab = btab))
  else
      return(s/ecct)
}





#' @details
#' Method for "igraph"s
#'
#' @param vattr character scalar or any vector of length equal to
#' \code{vcount(object)}, name of the vertex attribute in \code{object}
#' designating the groups or a vector with the attribute itself
#'
#' @method freeman igraph
#' @export
#' @rdname freeman
freeman.igraph <- function(object, vattr, dis=NULL, ...)
{
    stopifnot(!is.directed(object))
    m <- as.mixingm(object, vattr, full=TRUE)
    if(is.null(dis)) dis <- attr(m, "gsizes")
    freeman(m, dis=dis, ...)
}
