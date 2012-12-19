#' Odds ratio within-group tie existence
#'
#' Odds ratio for connected, as opposed to disconnected, dyads depedning
#' whether it is between or within-group, i.e. how much more likely the dyad
#' will be connected if it is within-group.
#'
#' The measure takes values, like all odds ratios, from (0; Inf).
#'
#' @param g object of class "igraph", the network
#'
#' @param vattr character, name of the vertex attribute
#'
#' @return Numeric value of the measure.
#'
#' @references
#' Moody, Jim (2001) "Race, school integration, and friendship segregation in
#' America", American Journal of Sociology, 107(3):679--377
#'
#' @export
#' @example examples/moody.R
#' @family segregation measures
moody <- function(g, vattr)
{
    m <- mixingm(g, vattr, full=TRUE)
    z <- apply(m, 3, function(x) 
        c(between= sum(x) - sum(diag(x)), within=sum(diag(x))) )
    offdiag <- z[ col(z) != row(z) ]
    prod(diag(z)) / prod(offdiag)
}
