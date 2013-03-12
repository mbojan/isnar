#' Spectral Segregation Index for Social Networks
#'
#' These functions implement Spectral Segregation Index as proposed by
#' Echenique & Fryer (2006). This index is a node-level measure of segregation
#' in a given network.
#'
#' For a full description and axiomatization see Echenique & Fryer (2006).
#'
#' The network \code{g} is converted to adjacency matrix and normalized so that
#' all rows sum-up to 1.
#'
#' The procedure essentially consists of creating a submatrix, say, \eqn{B} of
#' the adjacency matrix, say \eqn{A}. This submatrix \eqn{B} contains only
#' vertices of the given type. It may be viewed as a type-homogeneous
#' subnetwork of \code{A}. This subnetwork is further decomposed into connected
#' components.  Then, for every component, an eigenvalue decomposition is
#' applied.  The value of the index for the component is simply the largest
#' eigenvalue, and the individual-level indices are obtained by distributing it
#' according to the corresponding eigenvector.
#'
#' @param g object of class "igraph" representing a network
#'
#' @param vattr character, name of the vertex attribute
#'
#' @return
#' Named vector of individual level values of SSI. Names correspond to vertex
#' ids in \code{g}.
#'
#' @references
#' Echenique, Federico and Roland G. Fryer, Jr. (2006) A Measure of Segregation
#' Based On Social Interactions
#'
#' @family segregation measures
#' @export
#' @example examples/ssi.R
ssi <- function(g, vattr)
{
    stopifnot( !is.directed(g) )
    # edge weights are "directed"
    gg <- as.directed(g, mode="mutual")
    V(gg)$id <- V(g)
    # add edge weights, these are "directed"
    l <- get.adjedgelist(gg, mode="out")
    ll <- lapply(l, function(x) structure( rep(1/length(x), length(x)), names=x) )
    ul <- unlist(ll)
    E(gg)$weight <- ul[ order( as.numeric(names(ul)) ) ]
    rm(l, ll, ul) # clean-up
    a <- get.vertex.attribute(gg, vattr)
    l <- unlist(lapply(unique(a), function(val) ssib(g=gg, vattr=vattr, b=val)))
    l[ order(as.numeric(names(l))) ]
}


ssib <- function(g, vattr, b)
{
    # take subgraph of b-nodes
    ids <- get.vertex.attribute(g, vattr)
    sub <- induced.subgraph(g, which(ids == b))
    # get components
    comps <- decompose.graph(sub)
    # compute eigen-decomposition
    e <- lapply(comps, function(k) eigen( get.adjacency(k, attr="weight")))
    # component SSIs (largest eigenvalue)
    cssi <- sapply(e, function(x) max(as.double(x$values)) )
    # eigenvectors of largest eigenvalue
    ev <- lapply(e, function(x) as.double(x$vectors[, which.max(as.double(x$values))] ))
    issi <- unlist( lapply( seq(1, length(e)),
        function(i)
        {
            rval <- cssi[i] * ev[[i]] / mean(ev[[i]])
            names(rval) <- V(comps[[i]])$id
            rval
        } ) )
    issi = issi[ order(as.numeric(names(issi))) ]
}
