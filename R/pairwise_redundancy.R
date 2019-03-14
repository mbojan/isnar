#' Pairwise tie redundancy
#'
#' Measuring relative redundancy of ties.
#'
#' @param g graph
#' @param v nodes to be treated as egos
#'
#' @details
#'
#' \code{pairwise_redundancy} computes a combination of the two measures:
#'
#' @return
#' Every function returns a data frame with columns \code{ego}, \code{v1},
#' \code{v2} with codes for, respectively, ego, alter1, and alter2. Additional column
#' depends on function and it is:
#' \describe{
#' \item{invdis}{inverse distance returned by \code{pr_invdistance}}
#' \item{sharedp}{number of shared partners returned by \code{pr_sharedp}}
#' \item{pr_redundancy}{the two above combined returned by \code{pairwise_redundancy}}
#' }
#'
#' @export

pairwise_redundancy <- function(g, v=igraph::V(g)) {
  pr_invdistance(g=g, v=v) %>%
  left_join(
    pr_sharedp(g=g, v=v)
  ) %>%
    transmute_(
      ~ego, ~v1, ~v2,
      pr_redundancy = ~ifelse(invdis == 1, sharedp + 1, invdis)
    )
}







# Inverse geodesic distance ----------------------------

#' \code{pr_invdistance} calculates inverse geodesic distance between
#' alter \code{v1} and alter \code{v2} when all ego-alter ties are removed.
#'
#' @rdname pairwise_redundancy
#' @export
pr_invdistance <- function(g, v=igraph::V(g)) {
  bind_rows(
    lapply(v, function(x) pr_invdistance1(g, igraph::V(g)[x]) )
  )
}

pr_invdistance1 <- function(g, v) {
  stopifnot(length(v) == 1)
  stopifnot(inherits(v, "igraph.vs"))
  nam <- v$name
  e <- igraph::make_ego_graph(g, order=1, nodes=v)[[1]]
  name <- NULL # evade "undefined global variable"
  a <- igraph::delete_vertices(e, igraph::V(e)[name == nam]) %>%
    igraph::distances() %>%
    "^"(-1) %>%
    mouter::asdf("invdis", add_dimnames = TRUE, retval="df") %>%
    filter_( quote(.dim1 < .dim2)) %>%
    transmute_(
      ego = ~nam,
      v1 = ~Var1,
      v2 = ~Var2,
      ~invdis
    )
}






# shared partners --------------------------------------

#' \code{pr_sharedp} calculates number of shared partners of \code{v1} and
#' \code{v2} when all ego-alter ties are removed.
#'
#' @rdname pairwise_redundancy
#' @export
pr_sharedp <- function(g, v=igraph::V(g)) {
  bind_rows(
    lapply(v, function(x) pr_sharedp1(g, igraph::V(g)[x]) )
  )
}

pr_sharedp1 <- function(g, v) {
  stopifnot(length(v) == 1)
  stopifnot(inherits(v, "igraph.vs"))
  nam <- v$name
  e <- igraph::make_ego_graph(g, order=1, nodes=v)[[1]]
  name <- NULL # evade "undefined global variable"
  a <- igraph::delete_vertices(e, igraph::V(e)[name == nam]) %>%
    igraph::as_adjacency_matrix(sparse=FALSE)
  a %*% a %>%
    mouter::asdf("sharedp", add_dimnames = TRUE, retval="df") %>%
    filter_( quote(.dim1 < .dim2)) %>%
    transmute_(
      ego = ~nam,
      v1 = ~Var1,
      v2 = ~Var2,
      ~sharedp
    )
}




