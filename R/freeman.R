#' Generalized Freeman's segregation index
#'
#' Calculate generalized Freeman's segregation index for undirected netoworks
#' with arbitrary number of groups.
#'
#' Freeman's segregation index (Freeman, 1978) is designed to capture the
#' extent to which the defined types of vertices tend to have more edges with
#' vertices from the same type than with other types (segregation).  Formally,
#' the index compares the number of edges in the specified graph that connect
#' vertices of different type to the number of ``inter-type'' edges that would
#' be expected under pure random graph. High values indicate that there one
#' would expect much more inter-type edges if they are to be created without
#' considering the type of vertices, suggesting high segregation. Low values
#' indicate that lower segregation. Especially, the index is 0 for the full
#' graph (all possible edges are present).
#'
#' The index has some discontinuity as there are graph and type configurations
#' that are characterized by the higher number of inter-type edges that is
#' expected under a random graph. The index is truncates these situations and
#' takes a value of 0.
#'
#' The original Freeman's formulation was for only two types of vertices.  Here
#' it is extended to the arbitrary number of types. The modification only
#' affects the way in which the expected number of inter-type edges under pure
#' random graph is calculated.
#'
#' About the function itself:
#'
#' The function internally calculates the frequency of types of vertices in the
#' supplied attributer \code{vattr}. However, it is possible to override this
#' by specifying ``true'' type distribution with the \code{dis} argument. It is
#' assumed to be a table (as returned by \code{table}) or a numeric vector with
#' frequencies of types of vertices. This may be especially usefull when
#' dealing with large graphs with larger number of isolates.
#'
#' @param g object of class "igraph" representing the network
#'
#' @param vattr name of vertex attribute in \code{g}
#'
#' @param dis numeric, optional true distribution of types, see Details
#'
#' @param more logical, should some more output be returned
#'
#' @return 
#' The value of the Freeman's index.
#'
#' If \code{more} is \code{TRUE} some intermediate results returned in a list.
#'
#' @references
#' Freeman, Linton C. (1978) Segregation in Social Networks, \emph{Sociological
#' Methods & Research} \bold{6}(4):411--429
#'
#' @example examples/freeman.R
#' @keywords graphs
#' @export
#' @family segregation measures
freeman <- function(g, vattr, dis=NULL, more=FALSE)
{
    stopifnot(inherits(g, "igraph"))
    stopifnot(!is.directed(g))
    n <- ecount(g)
    m <- mixingm(g, vattr)
    # number of cross-group ties
    cct <- sum(m) - sum(diag(m))
    # group distribution
    if(is.null(dis))
        btab <- table( get.vertex.attribute(g, vattr))
    else
        btab <- dis
    m <- sum(btab)
    ecct <- (n * (sum(btab)^2 - sum(btab^2))) / (m * (m - 1))
    s <- ecct - cct
    if(s < 0)  s <- 0
    if(more)
        return(list(ecct = ecct, cct = cct, n = n, m = m, btab = btab))
    else
        return(s/ecct)
}

   

