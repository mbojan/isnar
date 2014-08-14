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
