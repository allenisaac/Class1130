## ab test

#permutation tests
set.seed(3)
grp.1 <- round(rlnorm(100,6)) # new page
grp.2 <- round(rlnorm(100,6)) # old page
# Null - distributions are the same

hist(grp.1)
hist(grp.2)

data <- c(grp.1, grp.2)

l1 <- length(grp.1)
l2 <- length(grp.2)
lt <- l1 + l2


# test statistic
test.diff <- median(grp.1) - median(grp.2)


it <- function(n){
  M = NULL
  for(i in 1:n){
    # looks at permutations of data, no replacement
    s = sample(data, lt, FALSE)
    # test statistic of each permutation, abitrary split
    m1 = median(s[1:l1]) - median(s[(l1+1):lt])
    # vector of test statistics
    M = c(M,m1)
  }
  return(M)
}

#10000 simulations to give approximate distribution
examples <- it(10000)

par(mfrow = c(1,1))
hist(examples, col = "red", breaks = 100, main = "Random Permutations")
abline(v=test.diff, col = "black", lwd = 4)


#p-value
(sum(examples<test.diff))/10000
