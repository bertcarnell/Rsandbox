require(lhs)


correlateLhsColumns <- function(targetSample, correlatedSample, rho, permutations)
{
  P <- sapply(1:permutations, function(x) order(runif(length(targetSample))))

  Z <- apply(P, 2, function(x) correlatedSample[x])
  
  cors <- apply(Z, 2, function(x) cor(x, targetSample))
  
  ind <- which.min((cors - rho)^2)

  tempold <- (cor(targetSample, correlatedSample) - rho)^2
  tempnew <- (cor(targetSample, Z[,ind]) - rho)^2

  if (tempold < tempnew) return(correlatedSample)
  else return(Z[,ind])
}

set.seed(1976)

X <- randomLHS(100, 2)
Y <- X
Y[,1] <- qnorm(X[,1], 2, 3)
Y[,2] <- qnorm(X[,2], 4, 8)

cor(Y)
cov(Y)
Y[,2] <- correlateLhsColumns(Y[,1], Y[,2], 0.4, 10000)
cor(Y)
cov(Y)




