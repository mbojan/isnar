(m <- matrix(1:4, 2, 2))
(f1 <- fold(m))
(f2 <- fold(m, "lower"))

stopifnot( all.equal(diag(m), diag(f1)) )
stopifnot( all.equal(diag(m), diag(f2)) )
stopifnot( all.equal(f1[1,2], m[2,1] + m[1,2]) )
stopifnot( all.equal(f2[2,1], m[2,1] + m[1,2]) )
