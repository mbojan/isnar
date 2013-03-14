# Method for tables
# object, full=TRUE/FALSE, gsizes
#
# If given a three-dimensional table, the method should:
# 1. Compute the group sizes and set the attribute
# 2. Compute network size and set the attribute
# 3. Use an argument to decide if the network is directed, otherwise NA
#
# If given a two-dimensional table and full=TRUE
# 1. Reconstruct non-contact layer provided group sizes as an argument
# 2. Compute network size and set the attribute
# 3. Set group sizes attribute
# 4. Set directed attribute based on argument
#
# If given a two-dimensional table and full=FALSE
# 1. Set group sizes attribute based on argument, or NA
# 2. Set network size or NA
# 3. Set directed attribute or NA
mixingm.table <- function(object, directed=NULL, gsizes=NULL,
                          size=ifelse(is.null(gsizes), NULL, sum(gsizes)) )
{
}

# three dimensional table
mmTable3d <- function(object, directed=TRUE, loops=FALSE)
{
  eamargin <- apply(object, 1:2, sum)
  if(directed)
  {
    if(loops)
    {
      gsizes <- sqrt(diag(eamargin))
    } else
    {
      gsizes <- 0.5 * (1 + sqrt(1 + 4*diag(eamargin)))
    }
  } else
  {
    gsizes <- 0.5 * (1 + sqrt(1 + 8*diag(eamargin)))
  }
  structure(object, directed=directed, size=sum(gsizes), group.sizes=gsizes,
            class=c("mixingm", "table"))
}



# two-dimensional table
mmTable2d <- function(object, gsizes=NULL,
                      size=ifelse(is.null(gsizes), NULL, sum(gsizes)),
                      full=FALSE, directed=TRUE, loops=FALSE)
{
  if(full)
  {
    stopifnot(!is.null(gsizes))
    # compute ego-alter margin 'mar'
    o <- outer(gsizes, gsizes, "*")
    if(directed)
    {
        mar <- o
        if(!loops)
        {
            diag(mar) <- diag(o) - gsizes
        }
    } else {
            mar <- o
            mar[ lower.tri(mar) ] <- 0
        if(loops)
        {
            diag(mar) <- (diag(o) + gsizes) / 2
        } else {
            diag(mar) <- (diag(o) - gsizes) / 2
        }
    }
    # build the result
    rval <- array(NA, dim=c(dim(object), 2))
    rval[,,2] <- object
    rval[,,1] <- mar - object
    # set dimension names
    if(is.null(dimnames(object)))
    {
      dimnames(rval) <- list(NULL, NULL, tie=c(FALSE, TRUE))
    } else
    {
      dimnames(rval) <- c( dimnames(object)[1:2], list(tie=c(FALSE, TRUE)))
    }
  } else
  {
    rval <- object
  }
    structure(rval, size=size, group.sizes=gsizes, directed=directed,
              class=c("mixingm", "table"))
}
