#' Judges network
#'
#' Network of judges from one of the Polish regional courts. Relation indicates 
#' which judges have ruled in at least one case together. This network is
#' a projection from bipartite network \code{\link{judge_net_bp}}.
#' 
#' Node attributes include gender and code of division.
#'
#' @docType data
#' @name judge_net
#'
#' @format
#' Object of class igraph of size 40, undirected, with predefined layout.
#'
#' @source
#' Own calculation based on \href{https://saos-test.icm.edu.pl}{SAOS}
#'
NULL
