% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/full_mm.R
\name{full_mm}
\alias{full_mm}
\title{Create a 3-dimensional mixingm matrix}
\usage{
full_mm(cl, gsizes, directed = TRUE, loops = FALSE)
}
\arguments{
\item{cl}{numeric matrix with contact layer of the mixing matrix}

\item{gsizes}{numeric vector or matrix with group sizes, see Details.}

\item{directed}{logical, whether the network is directed}

\item{loops}{logical, whether loops (self-ties) are allowed

Contact layer of the mixing matrix is a cross-classification of ties
according to the attributes of tie sender (ego) and tie receiver (alter).
Classically, the same attribute is used for ego and alter resulting in a
square mixing matrix. In such cases \code{cl} shoud be square with \code{cl[i,j]} being
the number of ties from actors in group \code{i} to actors in group \code{j}
and \code{gsizes} should be a numeric vector with number of nodes in each group.
Consequently, it must hold that \code{length(gsizes) == nrow(cl) == ncol(cl)}.

In more general case we can use different node attributes for ego and
alter. Then \code{cl} does not have to be square. In such cases
\code{gsizes} should be a cross-tabulation of nodes according to 
their values on both attributes. See Examples.}
}
\value{
\code{full_mm} returns a full three-dimenstional mixing matrix as an array
with \code{dim} attribute equal to \code{c( nrow(cl), ncol(cl), 2 )}.
}
\description{
Given contact layer of the mixing matrix compute a full 3-dimensional mixing
matrix by rebuilding the non-contact layer from supplied group sizes and
other arguments.
}
\examples{
### Square example

# Contact layer of the mixing matrix
mm1 <- matrix( c( 20, 10, 5,
                 12, 30, 10,
                 3, 11, 25 ),
              byrow=TRUE, ncol=3, nrow=3)
dimnames(mm1) <- list(ego=letters[1:3], alter=letters[1:3])
mm1

# Assuming some group sizes
gs1 <- c(a=9, b=12, c=10)

# Full mixing matrix
full_mm( mm1, gs1)




### Non-square example

# Mixing matrix
# Now using different attributes for ego and alter
mm2 <- cbind(mm1, c(20, 10, 5))
colnames(mm2) <- LETTERS[1:4]
names(dimnames(mm2)) <- c("ego", "alter")
mm2

# Create artificial distribution of attributes
set.seed(123)
a1 <- sample(letters[1:3], sum(gs1), replace=TRUE, prob=gs1/sum(gs1))
table(a1)
a2 <- sample(LETTERS[1:4], sum(gs1), replace=TRUE)
table(a2)
(x <- table(a1, a2))         # Cross-tablulation

# Full mixing matrix
full_mm( mm2, gsizes=x)
}
