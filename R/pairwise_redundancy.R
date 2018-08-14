#' Pairwise tie redundancy
#' 
#' @description Measuring relative redundancy of ties in neighborhoods.
#' 
#' @rdname pairwise_redundancy
#'
#' @param g igraph with vertex names
#' @param v vertex sequence of nodes to analyzed (as egos)
#' 
#' @description  Function [pr_invdistance()] calculates for each ego in `v` an
#'   inverse geodesic distance between alters `v1` and `v2` having all ego-alter
#'   ties removed.
#'   
#' @return [pr_invdistance()] returns a data frame with columns:
#' - `ego` -- name of ego vertex
#' - `v1` -- name of alter 1
#' - `v2` -- name of alter 2
#' - `invdis` -- inverse geodesic distance between nodes `v1` and `v2` in `g` having all ties of `ego` removed
#' 
#'   
#' @export
pr_invdistance <- function(g, v=igraph::V(g)) {
  dplyr::bind_rows(
    lapply(v, function(x) pr_invdistance1(g, igraph::V(g)[x]) )
  )
}

pr_invdistance1 <- function(g, v) {
  stopifnot(length(v) == 1)
  stopifnot(inherits(v, "igraph.vs"))
  nam <- v$name
  e <- igraph::make_ego_graph(g, order=1, nodes=v)[[1]]
  if(ecount(e) == 0) return(NULL)
  a <- igraph::delete_vertices(e, igraph::V(e)[name == nam]) %>%
    igraph::distances() %>%
    "^"(-1) %>%
    mouter::asdf("invdis", add_dimnames = TRUE, retval="df") %>%
    dplyr::filter(.dim1 < .dim2) %>%
    dplyr::transmute(
      ego = nam,
      v1 = Var1,
      v2 = Var2,
      invdis
    )
}


#' @rdname pairwise_redundancy
#'
#' @description Function [pr_sharedp()] calculates the number of shared partners of
#'   nodes `v1` and `v2` having all ties of `ego` removed.

#' \code{pr_sharedp} calculates number of shared partners of \code{v1} and
#' \code{v2} when all ego-alter ties are removed.
#'
#' @rdname pairwise_redundancy
#' @export
pr_sharedp <- function(g, v=igraph::V(g)) {
  dplyr::bind_rows(
    lapply(v, function(x) pr_sharedp1(g, igraph::V(g)[x]) )
  )
}

pr_sharedp1 <- function(g, v) {
  stopifnot(length(v) == 1)
  stopifnot(inherits(v, "igraph.vs"))
  nam <- v$name
  e <- igraph::make_ego_graph(g, order=1, nodes=v)[[1]]
  if(ecount(e) == 0) return(NULL)
  a <- igraph::delete_vertices(e, igraph::V(e)[name == nam]) %>%
    igraph::as_adjacency_matrix(sparse=FALSE)
  a %*% a %>%
    mouter::asdf("sharedp", add_dimnames = TRUE, retval="df") %>%
    dplyr::filter(.dim1 < .dim2) %>%
    dplyr::transmute(
      ego = nam,
      v1 = Var1,
      v2 = Var2,
      sharedp
    )
}


#' @rdname pairwise_redundancy
#' 
#' @description Function [pairwise_redundancy()] computes a combination of the to measures.
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
    dplyr::left_join(
      pr_sharedp(g=g, v=v),
      by = c("ego", "v1", "v2")
    ) %>%
    dplyr::transmute(
      ego, v1, v2,
      pr_redundancy = ifelse(invdis == 1, sharedp + 1, invdis)
    )
}
