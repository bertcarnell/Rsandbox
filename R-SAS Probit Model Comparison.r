# probit model maximum likelihood algorithm

# try to make R match SAS functionality in probit model fits

###### utility functions ###########
probit.loglik <- function(param, x, success, failure)
{
  # param[1] is the intercept
  # param[2] is the slope
  p <- pnorm(param[1]+param[2]*x)
  loglik <- sum(success*log(p)) +
    #sum(X$Failure*log(1-p))
    sum(failure*log1p(-p))
  return(-1*loglik)
}

probit.loglik.adj <- function(param, x, success, failure)
{
  # param[1] is the intercept
  # param[2] is the slope
  p <- pnorm(param[1]+param[2]*x)
  p[p == 1] <- 0.9999999999999999 # .Machine$double.eps =  2.220446e-16
  loglik <- sum(success*log(p)) +
    sum(failure*log1p(-p))
  return(-1*loglik)
}

# y is the response variable.  The number of successes
# n is the total trials at a dose level
# x is the dose level (usually log transformed)

fit.ml.probit <- function(y, n, x, start.intercept=1, start.slope=1,
  bTrace=FALSE, ci.alpha=0.05, optim.method="Nelder-Mead")
{
  stopifnot(length(y) == length(n))
  stopifnot(length(y) == length(x))
  o <- optim(c(start.intercept,start.slope), probit.loglik.adj, x=x, success=y,
    failure=n-y, hessian=TRUE, control=list(trace=bTrace), method=optim.method)
  vcov.matrix <- solve(o$hessian)
  # http://en.wikipedia.org/wiki/Fieller's_theorem
  a <- -1*o$par[1]
  b <- o$par[2]
  v11 <- vcov.matrix[1,1]
  v22 <- vcov.matrix[2,2]
  v12 <- -1*vcov.matrix[1,2]
  d50 <- a/b
  g <- qnorm(1-ci.alpha/2)^2*v22/b^2
  ml <- 1/(1-g)*(a/b-g*v12/v22-qnorm(1-ci.alpha/2)/b*sqrt(v11-2*a/b*v12+a^2/b^2*v22-g*(v11-v12^2/v22)))
  mu <- 1/(1-g)*(a/b-g*v12/v22+qnorm(1-ci.alpha/2)/b*sqrt(v11-2*a/b*v12+a^2/b^2*v22-g*(v11-v12^2/v22)))
  p1 <- 2*(1-pnorm(abs(a)/sqrt(v11)))
  p2 <- 2*(1-pnorm(abs(b)/sqrt(v22)))

  return(list(intercept=o$par[1], slope=o$par[2], loglikelihood=-1*o$value,
    vcov.matrix=vcov.matrix, se=sqrt(diag(vcov.matrix)), pvalue=c(p1,p2),
    d50=d50, d50.ci=c(ml, mu)))
}

#fit.ml.probit.newton <- function(y, n, x, start.intercept=1, start.slope=1,
#  bTrace=FALSE, ci.alpha=0.05)
#{
#  stopifnot(length(y) == length(n))
#  stopifnot(length(y) == length(x))
#  o <- nlm(probit.loglik.adj, c(start.intercept,start.slope), x=x, success=y,
#    failure=n-y, hessian=TRUE, print.level=ifelse(bTrace,2,0), ndigit=16,
#    steptol=1E-12, gradtol=1E-12)
#  return(o)
#}

# SAS V8, proc probit, example 54.1 Dosage Levels

#Model Information
#Data Set WORK.B
#Events Variable Response
#Trials Variable N
#Number of Observations 7
#Number of Events 38
#Number of Trials 74
#Missing Values 29
#Name of Distribution NORMAL
#Log Likelihood -37.28038802
#
#Probit Procedure
#Analysis of Parameter Estimates
#                    Standard
#Variable DF Estimate Error Chi-Square Pr > ChiSq Label
#Intercept 1 -1.81270 0.44934 16.2743 <.0001 Intercept
#Log10(Dose) 1 3.41812 0.74555 21.0196 <.0001

X <- data.frame(Dose=1:7,
  N=c(10,12,10,10,12,10,10),
  Response=c(1,2,4,5,8,8,10))
X$lDose <- log10(X$Dose)

g <- fit.ml.probit(X$Response, X$N, X$lDose)
g
10^g$d50
10^g$d50.ci

abs(g$intercept-(-1.81270))/1.81270*100
abs(g$slope-3.41812)/3.41812*100
abs(g$loglikelihood-(-37.28038802))/37.28038802*100
abs(10^g$d50 - 10^(1.81270/3.41812))/10^(1.81270/3.41812)*100

#g <- fit.ml.probit.newton(X$Response, X$N, X$lDose, bTrace=TRUE)


## 0 and 1 response example

X <- data.frame(Dose=2^(0:9), Response=c(rep(0,5), rep(4,5)),
  N=rep(4,10))
X$lDose <- log10(X$Dose)

g <- fit.ml.probit(X$Response, X$N, X$lDose, optim.method="SANN")
g
10^g$d50
10^g$d50.ci

# wanted the geometric mean for d50
10^((log10(16)+log10(32))/2)

################################################################################

require(maxLik)

probit.loglik <- function(param, x, success, failure)
{
  # param[1] is the intercept
  # param[2] is the slope
  p <- pnorm(param[1]+param[2]*x)
  loglik <- sum(success*log(p)) +
    #sum(X$Failure*log(1-p))
    sum(failure*log1p(-p))
  return(loglik)
}

probit.loglik.adj <- function(param, x, success, failure)
{
  # param[1] is the intercept
  # param[2] is the slope
  p <- pnorm(param[1]+param[2]*x)
  p[p == 1] <- 0.9999999999999999 # .Machine$double.eps =  2.220446e-16
  loglik <- sum(success*log(p)) +
    sum(failure*log1p(-p))
  return(loglik)
}

X <- data.frame(Dose=1:7,
  N=c(10,12,10,10,12,10,10),
  Response=c(1,2,4,5,8,8,10))
X$lDose <- log10(X$Dose)

maxNR(probit.loglik, start=c(1,1), x=X$lDose, success=X$Response, failure=X$N-X$Response)
maxNR(probit.loglik.adj, start=c(1,1), x=X$lDose, success=X$Response, failure=X$N-X$Response)

X <- data.frame(Dose=2^(0:9), Response=c(rep(0,5), rep(4,5)),
  N=rep(4,10))
X$lDose <- log10(X$Dose)

#maxNR(probit.loglik, start=c(1,1), x=X$lDose, success=X$Response, failure=X$N-X$Response) # Fails
mnr <- maxNR(probit.loglik.adj, start=c(1,1), x=X$lDose, success=X$Response, failure=X$N-X$Response)
10^(-mnr$estimate[1]/mnr$estimate[2])
