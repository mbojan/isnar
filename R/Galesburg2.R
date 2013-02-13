#' Columbia University Drug Study data 2
#' 
#' Data from Columbia University Drug Study on diffusion of medical innovation
#' in the form of a new drug (gammanym). Its a network of 31 medical doctors
#' connected with friendship or discussion links. Vertex attribute "adoption"
#' specifies time of first prescription of the new drug.
#'
#' @docType data
#' @name Galesburg2
#'
#' @format
#' Directed network (class 'igraph') with edge attributes:
#' \describe{
#' \item{discussion}{logical, discussion link}
#' \item{friendship}{logical, friendship nomination}
#' }
#' and vertex attributes:
#' \describe{
#' \item{adoption}{numeric, time of adoption}
#' }
#'
#' @source
#' Pajek datasets
#' \url{http://vlado.fmf.uni-lj.si/pub/networks/data/esna/Galesburg2.htm}
#'
#' @references
#' Coleman, J.S., E. Katz, H. Menzel (1966) "Medical Innovation. A Diffusion
#' Study", Indianapolis: Bobbs-Merrill
#'
#' @example examples/Galesburg2.R
#'
NULL
