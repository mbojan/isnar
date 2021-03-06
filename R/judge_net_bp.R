#' Bipartite judges--judgments network
#'
#' Two-mode network with classes representing judges and judgments from one of 
#' the Polish regional courts. Relation indicates which judges were involved in
#' each case.
#' 
#' Node attributes include judges' gender and code of division. Attribute 'type'
#' indicates classes of nodes, in accordance with igraph representation of 
#' bipartite networks, \code{TRUE} for judges and \code{FALSE} for judgments.
#'
#' @docType data
#' @name judge_net_bp
#'
#' @format
#' Object of class igraph of size 1189, undirected, bipartite.
#'
#' @source
#' Own calculation based on \href{https://saos-test.icm.edu.pl}{SAOS}
#'
NULL
