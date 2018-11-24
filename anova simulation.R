
set.seed(1976)
N <- 1000
Xmeans <- c(1, 2, 10, 0, 1, 0)
X <- data.frame(X1=rnorm(N, Xmeans[1], 1.5),
                X2=rlnorm(N, Xmeans[2], 0.5),
                X3=rnorm(N, Xmeans[3], 3),
                X4=rnorm(N, Xmeans[4], 1),
                X5=rnorm(N, Xmeans[5], 2),
                X6=rnorm(N, Xmeans[6], 10))
Y <- cbind(5+2*X$X1+3*X$X2, 
           1+3*X$X1, 
           2+X$X1+4*X$X2+8*X$X3+2*X$X2*X$X3)
means <- c(5+2*Xmeans[1]+3*Xmeans[2], 1+3*Xmeans[1], 2+Xmeans[1]+4*Xmeans[2]+8*Xmeans[3]+2*Xmeans[2]*Xmeans[3])
sigmas <- var(Y)

Z <- apply(Y, 1, function(y) ifelse(y[1] > 50 | y[2] > 10 | y[3] < 80, 1, 0))
sum(Z)

require(randomForest)
set.seed(2001)
rf1 <- randomForest(X, as.factor(Z), strata=as.factor(Z), classwt=c(0.5, 0.5))
rf1$importance

auc1 <- plot.roc(Z, as.numeric(predict(rf1))-1, col="red")

g1 <- glm(Z ~ (X1+X2+X3)^2, data=X, family=binomial)
g2 <- step(g1, direction="both")

auc2 <- plot.roc(Z, predict(g2, type="response"), col="blue")

mlm <- lm(update(formula(g2), "Y~."), data=X)
summary(mlm) # 3 models
manova(mlm)

pmlm <- predict(mlm)
predict(mlm, newdata=X[1,], interval="prediction")


cov(pmlm)
cov(Y)
W <- apply(pmlm, 1, function(y) ifelse(y[1] > 50 | y[2] > 10 | y[3] < 80, 1, 0))
table(Z, W)

predict(mlm, newdata=X[1,])
predict(mlm, newdata=X[1,], interval="confidence")

mlm2 <- lm(Y~(X1+X2+X3+X4+X5+X6)^2, data=X)
manova(mlm2)
mlm3 <- lm(Y~X1+X2+X3+X4+X5+X6, data=X)
manova(mlm3)
#mlm4 <- step(manova(mlm3), direction="both", scope=c(formula(mlm2), formula(mlm3)))

require(car)
Mlm2 <- Manova(mlm2)
summary(Mlm2)
Mlm3 <- Manova(mlm3)
summary(Mlm3)

invTranPlot(X$X1, Y[,1], lambda=c(0,1,2,3), family="yjPower")
invTranPlot(X$X1, Y[,2], lambda=c(0,1,2,3), family="yjPower")
invTranPlot(X$X1, Y[,3], lambda=c(0,1,2,3), family="yjPower")

invTranPlot(X$X2, Y[,1])
invTranPlot(X$X2, Y[,2])
invTranPlot(X$X2, Y[,3])
invTranEstimate(X$X2, Y[,1])

powerTransform(mlm2, family="yjPower")

scatterplot(Y[,1] ~ X$X1 | Z, ellipse=TRUE)
scatterplot(Y[,1] ~ X$X3 | Z, ellipse=TRUE)

windows(h=7, w=7)
scatterplotMatrix(~Y[,1]+Y[,2] + Y[,3] +X1 + X2 + X3 + X4 + X5 + X6 | Z, ellipse=TRUE, data=X)


require(pls)
pls1 <- plsr(Y~(X1+X2+X3+X4+X5+X6)^2, data=X, ncom=8)

#######################################################################

N <- 1000
Xts <- data.frame(X1=numeric(4*N), X2=numeric(4*N), X3=numeric(4*N),
                  X4=numeric(4*N), X5=numeric(4*N), X6=numeric(4*N))
Xts[1,] <- c(1,1,1,1,1,1)
set.seed(1976)
for (i in 2:N)
{
  Xts[i,] <- c(Xts[i-1,1] + rnorm(1, 0.25, 2),
               Xts[i-1,2] + rnorm(1, 0.5, 2),
               0.9*Xts[i-1,3] - rnorm(1, 0.5, 2),
               rnorm(1, 0, 1),
               rnorm(1, 2, 2),
               rnorm(1, -0.5, 1))
}

Xts[N+1,] <- c(3,3,3,1,1,1)
for (i in (N+2):(2*N))
{
  Xts[i,] <- c(Xts[i-1,1] + rnorm(1, 0.25, 5),
               Xts[i-1,2] + rnorm(1, 0.5, 5),
               0.9*Xts[i-1,3] - rnorm(1, 0.5, 5),
               rnorm(1, 0, 1),
               rnorm(1, 2, 2),
               rnorm(1, -0.5, 1))
}

Xts[2*N+1,] <- c(-3,-3,-3,1,1,1)
for (i in (2*N+2):(3*N))
{
  Xts[i,] <- c(Xts[i-1,1] + rnorm(1, 0.3, 2),
               Xts[i-1,2] + rnorm(1, 0.6, 2),
               0.9*Xts[i-1,3] - rnorm(1, 0.7, 2),
               rnorm(1, 0, 1),
               rnorm(1, 2, 2),
               rnorm(1, -0.5, 1))
}

Xts[3*N+1,] <- c(9,9,9,1,1,1)
for (i in (3*N+2):(4*N))
{
  Xts[i,] <- c(Xts[i-1,1] + rnorm(1, 0.3, 2),
               Xts[i-1,2] + rnorm(1, 0.7, 2),
               0.9*Xts[i-1,3] - rnorm(1, 1, 2),
               rnorm(1, 0, 1),
               rnorm(1, 2, 2),
               rnorm(1, -0.5, 1))
}

Yts <- matrix(NA, nrow=4*N, ncol=3)
Yts[1,] <- c(0,0,0)
for (i in 2:(4*N))
{
  Yts[i,] <- c(5+2*Xts$X1[i]+3*Xts$X2[i]+0.2*Yts[i-1,1]+rnorm(1,0,1), 
               1+3*Xts$X1[i] + 0.25*Yts[i-1,2]+rnorm(1,0,1), 
               2+Xts$X1[i]+4*Xts$X2[i]+8*Xts$X3[i]+2*Xts$X2[i]*Xts$X3[i]+0.3*Yts[i-1,3]+rnorm(1,0,1))
}

apply(Yts, 2, summary)

Zts <- apply(Yts, 1, function(y) ifelse(y[1] > 3000 | y[2] > 1000 | y[3] < -20000, 1, 0))
sum(Zts)

pat <- rep(1:4, each=N)

set.seed(2001)
require(randomForest)
require(pROC)
require(nlme)
rf1 <- randomForest(Xts, as.factor(Zts), strata=as.factor(Zts), classwt=c(0.5, 0.5))
rf1$importance

auc1 <- plot.roc(Zts, as.numeric(predict(rf1))-1, col="red")

g1 <- glm(Zts ~ (X1+X2+X3)^2, data=Xts, family=binomial)
g2 <- step(g1, direction="both")

auc2 <- plot.roc(Zts, predict(g2, type="response"), col="blue", add=TRUE)

fm1 <- gls(Yts[,1] ~ (X1+X2+X3)^2, data=Xts,
           correlation = corAR1(value=0.4, form = ~ 1 | pat))
plot(fm1, col=pat)
pfm1 <- predict(fm1, newdata=Xts)

fm2 <- gls(Yts[,2] ~ (X1+X2+X3)^2, data=Xts,
           correlation = corAR1(value=0.4, form = ~ 1 | pat))
plot(fm2, col=pat)
pfm2 <- predict(fm1, newdata=Xts)

fm3 <- gls(Yts[,3] ~ (X1+X2+X3)^2, data=Xts,
           correlation = corAR1(value=0.4, form = ~ 1 | pat))
plot(fm3, col=pat)
pfm3 <- predict(fm1, newdata=Xts)

pZts <- apply(cbind(pfm1, pfm2, pfm3), 1, function(y) ifelse(y[1] > 3000 | y[2] > 1000 | y[3] < -20000, 1, 0))
table(Zts, pZts)

auc1 <- plot.roc(Zts, as.numeric(predict(rf1))-1, col="red")


