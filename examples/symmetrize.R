m <- matrix(1:16, 4, 4)

# copy upper triangle onto lower symmetrically
symmetrize(m, "upper")

# copy lower triangle onto upper symmetrically
symmetrize(m, "lower")

# distribute off-diagonal values exactly
# r[i,j] = (m[i,j] + m[j,i]) / 2
r1 <- symmetrize(m, "div")
r1
all.equal(sum(m), sum(r1))

# distribute off-diagonal values using integer division
r2 <- symmetrize(m, "intdiv")
r2
all.equal(sum(m), sum(r2))
