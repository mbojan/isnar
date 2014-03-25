\docType{data}
\name{Galesburg2}
\alias{Galesburg2}
\title{Columbia University Drug Study data 2}
\format{Directed network (class 'igraph') with edge attributes:
\describe{
\item{discussion}{logical, discussion link}
\item{friendship}{logical, friendship nomination}
}
and vertex attributes:
\describe{
\item{adoption}{numeric, time of adoption}
}}
\source{
Pajek datasets
\url{http://vlado.fmf.uni-lj.si/pub/networks/data/esna/Galesburg2.htm}
}
\description{
Data from Columbia University Drug Study on diffusion of
medical innovation in the form of a new drug (gammanym).
Its a network of 31 medical doctors connected with
friendship or discussion links. Vertex attribute "adoption"
specifies time of first prescription of the new drug.
}
\examples{
data(Galesburg2)
plot(Galesburg2, edge.arrow.size=.3)
}
\references{
Coleman, J.S., E. Katz, H. Menzel (1966) "Medical
Innovation. A Diffusion Study", Indianapolis: Bobbs-Merrill
}

