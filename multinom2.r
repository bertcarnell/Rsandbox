require(nnet)
require(mlogit)
ilogit <- function(z) exp(z) / ( 1 + exp(z) )
require(mvtnorm)
rdirichlet <- function(n,a) 
{
  X <- sapply(a, function(y) rgamma(n, y, 1))
  apply(X, 2, "/", rowSums(X))
}

#X <- data.frame(
#  munition=as.factor(c("RPG1","RPG1","RPG1","RPG1",
#                       "RPG2","RPG2","RPG2","RPG2",
#                       "AP","AP","AP","AP")),
#  RDX=as.factor(c("Y","Y","Y","Y", "Y","N","N","N", "N","N","N","N")),
#  HMX=as.factor(c("Y","Y","Y","N", "Y","Y","Y","N", "N","N","N","N")))

set.seed(1976)  
Ndata <- 100  
X <- data.frame(
  munition=as.factor(c(rep("RPG1", Ndata), rep("RPG2", Ndata), rep("AP", Ndata))),
  RDX=as.factor(c(rbinom(Ndata,1,0.5), rbinom(Ndata,1,0.1), rbinom(Ndata,1,0.9))),
  HMX=as.factor(c(rbinom(Ndata,1,0.1), rbinom(Ndata,1,0.5), rbinom(Ndata,1,0.9))),
  TNT=as.factor(c(rbinom(Ndata,1,0.9), rbinom(Ndata,1,0.1), rbinom(Ndata,1,0.5))),
  PETN=as.factor(c(rbinom(Ndata,1,0.1), rbinom(Ndata,1,0.1), rbinom(Ndata,1,0.1))))

################################################################################
# Fit an mlogit model
################################################################################
   
Y <- mlogit.data(X, shape = "wide", choice = "munition")
test <- sapply(X, as.numeric, USE.NAMES=FALSE)
dimnames(test)[[2]] <- NULL
test <- test - 1
test[,1] <- 1 

mlm <- mlogit(munition ~0 | RDX + HMX + TNT + PETN, data = Y)
summary(mlm)
vcov(mlm)
coef(mlm)

test.result <- cbind(
  rep(1,length(X$munition)), # intercept
  exp(test %*% coef(mlm)[grep("RPG1", names(coef(mlm)))]), # RPG1 variables
  exp(test %*% coef(mlm)[grep("RPG2", names(coef(mlm)))]) # RPG2 variables
)
temp1 <- apply(test.result, 2, "/", rowSums(test.result))
temp2 <- mlm$fitted.values
stopifnot(all(temp1-temp2 < 1E-6))

################################################################################
# try a multivariate multinomial model
################################################################################

# doesn't work - binomial thinks this is count data
#g <- glm(test[,2:3] ~ X$munition, family=binomial(link="logit"))
#g
g1 <- glm(X$RDX ~ X$munition, family=binomial(link="logit"))
g2 <- glm(X$HMX ~ X$munition, family=binomial(link="logit"))

################################################################################
# try the MLE
################################################################################

idrpg1 <- numeric(length(X$munition))
idrpg2 <- numeric(length(X$munition))
idrpg1[X$munition=="RPG1"] <- 1
idrpg2[X$munition=="RPG2"] <- 1
likelihoodDataGivenExplosive <- function(explosiveType, coefs)
{
  -sum(log(  ilogit(coefs[1] + coefs[2]*idrpg1[explosiveType==1] + coefs[3]*idrpg2[explosiveType==1]))) - 
   sum(log(1-ilogit(coefs[1] + coefs[2]*idrpg1[explosiveType==0] + coefs[3]*idrpg2[explosiveType==0])))
}
f <- function(a) 
{
  # a is intercept.rdx, rpg1, rpg2, intercept.hmx, rpg1, rpg2
   likelihoodDataGivenExplosive(X$RDX, a[1:3]) +
   likelihoodDataGivenExplosive(X$HMX, a[4:6]) +
   likelihoodDataGivenExplosive(X$TNT, a[7:9]) +
   likelihoodDataGivenExplosive(X$PETN, a[10:12])
}
#  -sum(log(  ilogit(a[1]  + a[2]*idrpg1[X$RDX==1]   +  a[3]*idrpg2[X$RDX==1]))) - 
#   sum(log(1-ilogit(a[1]  + a[2]*idrpg1[X$RDX==0]   +  a[3]*idrpg2[X$RDX==0]))) -
#   sum(log(  ilogit(a[4]  + a[5]*idrpg1[X$HMX==1]   +  a[6]*idrpg2[X$HMX==1]))) - 
#   sum(log(1-ilogit(a[4]  + a[5]*idrpg1[X$HMX==0]   +  a[6]*idrpg2[X$HMX==0]))) -
#   sum(log(  ilogit(a[7]  + a[8]*idrpg1[X$TNT==1]   +  a[9]*idrpg2[X$TNT==1]))) -
#   sum(log(1-ilogit(a[7]  + a[8]*idrpg1[X$TNT==0]   +  a[9]*idrpg2[X$TNT==0]))) -
#   sum(log(  ilogit(a[10] + a[11]*idrpg1[X$PETN==1] + a[12]*idrpg2[X$PETN==1]))) -
#   sum(log(1-ilogit(a[10] + a[11]*idrpg1[X$PETN==0] + a[12]*idrpg2[X$PETN==0])))
#}

r <- optim(rep(0.5, 12), f, control=list(maxit=1000), hessian=TRUE)

test.rdx <- matrix(c(1,0,0,0,0,0,0,0,0,0,0,0, # RDX | AP
                     1,1,0,0,0,0,0,0,0,0,0,0, # RDX | RPG1
                     1,0,1,0,0,0,0,0,0,0,0,0), # RDX | RPG2
                     nrow=3, byrow=TRUE)
ilogit(test.rdx %*% r$par)/sum(ilogit(test.rdx %*% r$par)) # P(RDX | AP, RPG1, RPG2)
test.hmx <- matrix(c(0,0,0,1,0,0,0,0,0,0,0,0,
                     0,0,0,1,1,0,0,0,0,0,0,0,
                     0,0,0,1,0,1,0,0,0,0,0,0), nrow=3, byrow=TRUE)
ilogit(test.hmx %*% r$par)/sum(ilogit(test.hmx %*% r$par)) # P(HMX | AP, RPG1, RPG2)
test.rdxhmx <- matrix(c(1,0,0,1,0,0,0,0,0,0,0,0,
                        1,1,0,1,1,0,0,0,0,0,0,0,
                        1,0,1,1,0,1,0,0,0,0,0,0), nrow=3, byrow=TRUE)
ilogit(test.rdxhmx %*% r$par)/sum(ilogit(test.rdxhmx %*% r$par)) # P(RDX & HMX | AP, RPG1, RPG2)


# P(A | X) = P(A) * P(X | A)
# prior on AP, RPG1, RPG2
prior <- c(.05, .1, .85)
# data observed = RDX
posterior <- prior * ilogit(test.rdx %*% r$par)/sum(ilogit(test.rdx %*% r$par))
posterior / sum(posterior)
# data observed = RDX and HMX
posterior <- prior * ilogit(test.rdxhmx %*% r$par)/sum(ilogit(test.rdxhmx  %*% r$par))
posterior / sum(posterior)

# uncertainty in estimates 
Z <- sapply(1:length(r$par), function(x) rnorm(10000, r$par[x], sqrt(diag(r$hessian))))

Z2 <- apply(Z, 1, function(z) c(ilogit(test.rdxhmx %*% z)/sum(ilogit(test.rdxhmx %*% z))))
apply(Z2, 1, mean)
ilogit(test.rdxhmx %*% r$par)/sum(ilogit(test.rdxhmx %*% r$par)) # P(RDX & HMX | AP, RPG1, RPG2)
apply(Z2, 1, var)


Z2 <- cbind(
  ilogit(test.rdx %*% r$par)/sum(ilogit(test.rdx %*% r$par)) # P(RDX | AP, RPG1, RPG2)

  exp(Z[,1] + Z[,2]*test$RDX[1] + Z[,3]*test$HMX[1]),
  exp(Z[,4] + Z[,5]*test$RDX[1] + Z[,6]*test$HMX[1]),
  exp(rep(0, 100000)))
rowsums <- rowSums(Z2)
ind <- which(!is.finite(rowsums))
Z3 <- apply(Z2[-ind,], 2, "/", rowsums[-ind])
apply(Z3, 2, mean)
apply(Z3, 2, median)

Z4 <- cbind(
  rgamma(100000, exp(coef(m)[1,1] + coef(m)[1,2]*test$RDX[1] + coef(m)[1,3]*test$HMX[1]), 1),
  rgamma(100000, exp(coef(m)[2,1] + coef(m)[2,2]*test$RDX[1] + coef(m)[2,3]*test$HMX[1]), 1),
  rep(1, 100000))
rowsums <- rowSums(Z4)
Z5 <- apply(Z4, 2, "/", rowsums)
apply(Z5, 2, mean)

boxplot(log10(Z2[-ind]))
boxplot(Z4)


################################################################################

m <- multinom(munition ~ RDX + HMX + TNT + PETN, data = X)
predict(m)
predict(m, type="probs")

dimnames(test)[[2]] <- names(X)

test.result <- cbind(
  exp(rep(0, length(test[,1]))),
  as.numeric(exp(coef(m)[1,] %*% t(test))),
  as.numeric(exp(coef(m)[2,] %*% t(test)))
)
#  exp(coef(m)[1,1] + coef(m)[1,2]*test[,1] + coef(m)[1,3]*test[,2]) + coef(m)[1,4]*test,
#  exp(coef(m)[2,1] + coef(m)[2,2]*test[,1] + coef(m)[2,3]*test[,2]),
#  exp(rep(0, length(test[,1]))))
test1 <- apply(test.result, 2, "/", rowSums(test.result))
test2 <- predict(m, type="probs")
stopifnot(all(abs(test1 - test2) < 1E-6))

# assume parameters are multivariate normal
Z <- rmvnorm(100000, mean=c(coef(m)[1,], coef(m)[2,]),
  sigma=vcov(m))
coef(m)
apply(Z, 2, mean)
vcov(m)
cov(Z)

Z2 <- cbind(
  exp(Z[,1] + Z[,2]*test$RDX[1] + Z[,3]*test$HMX[1]),
  exp(Z[,4] + Z[,5]*test$RDX[1] + Z[,6]*test$HMX[1]),
  exp(rep(0, 100000)))
rowsums <- rowSums(Z2)
ind <- which(!is.finite(rowsums))
Z3 <- apply(Z2[-ind,], 2, "/", rowsums[-ind])
apply(Z3, 2, mean)
apply(Z3, 2, median)

Z4 <- cbind(
  rgamma(100000, exp(coef(m)[1,1] + coef(m)[1,2]*test$RDX[1] + coef(m)[1,3]*test$HMX[1]), 1),
  rgamma(100000, exp(coef(m)[2,1] + coef(m)[2,2]*test$RDX[1] + coef(m)[2,3]*test$HMX[1]), 1),
  rep(1, 100000))
rowsums <- rowSums(Z4)
Z5 <- apply(Z4, 2, "/", rowsums)
apply(Z5, 2, mean)

boxplot(log10(Z2[-ind]))
boxplot(Z4)

m <- multinom(X[,2:5] ~ munition, data = X)
