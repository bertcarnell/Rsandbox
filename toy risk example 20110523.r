set.seed(1976)

# LHC sample
N <- 500
# scenaio samples
M <- 100

# Theta_T
a1 <- 1.5
b1 <- 2
a2 <- 2
b2 <- 9
a3 <- 2
b3 <- 3

# Theta_C
mu1 <- runif(N,1,2)
mu2 <- runif(N,1,3)
mu3 <- runif(N,1,2.5)
mu4 <- runif(N,2,2.5)
s1 <- runif(N,0.1,0.5)
s2 <- runif(N,0.1,0.5)
s3 <- runif(N,0.1,0.5)
s4 <- runif(N,0.1,0.5)

# p(s)
p1 <- rbeta(N,a1,b1)
p2 <- rbeta(N,a2,b2)
p3 <- rbeta(N,a3,b3)

f <- function(a, b)
{
  windows()
  curve(dbeta(x, a, b), col="red", xlab="Branch Probability", ylab="Density")
  curve(dbeta(1-x, a, b), col="blue", add=TRUE)
}
f(a1,b1)
f(a2,b2)
f(a3,b3)

g <- function(m,si)
{
  windows()
  curve(dlnorm(x, m[1], si[1]), from=0, to=50, col="blue", xlab="Consequence",
    ylab="Density", ylim=c(0,2))
  for (i in 2:N)
  {
    curve(dlnorm(x, m[i], si[i]), from=0, to=50, col="blue", add=TRUE,
      ylim=c(0,50))
  }
  windows()
  curve(dlnorm(x, m[1], si[1]), from=1, to=50, col="blue", xlab="Consequence",
    ylab="Density", log="x", ylim=c(0,2))
  for (i in 2:N)
  {
    curve(dlnorm(x, m[i], si[i]), from=1, to=50, col="blue", add=TRUE, log="x",
      ylim=c(0,2))
  }
}

g(mu1,s1)
g(mu2,s2)
g(mu3,s3)
g(mu4,s4)

histogramBins <- c(0,10^(seq(0,3,by=0.5)))
histogramMids <- c(0,10^(seq(0.25, 3, by=0.5)))
# riskResultsA is an array of risk results for 4 scenarios,
#  with N uncertainty samples and a histogram of the m 
#  scenario monte carlo samples
riskResultsA <- array(NA, dim=c(4, N, length(histogramMids)))
for (i in 1:N)
{
  h1 <- hist(rlnorm(M,mu1[i],s1[i]), breaks=histogramBins, plot=FALSE)
  h2 <- hist(rlnorm(M,mu2[i],s2[i]), breaks=histogramBins, plot=FALSE)
  h3 <- hist(rlnorm(M,mu3[i],s3[i]), breaks=histogramBins, plot=FALSE)
  h4 <- hist(rlnorm(M,mu4[i],s4[i]), breaks=histogramBins, plot=FALSE)
  riskResultsA[1,i,] <- p1[i]*p2[i]*h1$counts/M
  riskResultsA[2,i,] <- p1[i]*(1-p2[i])*h2$counts/M
  riskResultsA[3,i,] <- (1-p1[i])*p3[i]*h3$counts/M
  riskResultsA[4,i,] <- (1-p1[i])*(1-p3[i])*h4$counts/M
}
# risk for all scenario aggregated
riskResults <- apply(riskResultsA, c(2,3), sum)

# risk PDf
windows()
plot(c(0.1,0.1),c(1,1), xlim=c(0.3,max(histogramBins)), ylim=c(0,1),
  ylab="Probability", xlab="Consequence", axes=FALSE, type="n", log="x")
axis(2)
axis(1, at=c(0.3,histogramBins[2:length(histogramBins)]),
  labels=c(0,round(histogramBins[2:length(histogramBins)],0)))
for (i in 1:N)
{
  lines(c(0.3,histogramBins[2]), rep(riskResults[i,1],2))
  for (j in 2:(length(histogramBins)-1))
  {
    lines(histogramBins[j:(j+1)], rep(riskResults[i,j],2))
  }
}
for (j in 2:(length(histogramBins)-1))
{
  boxplot(riskResults[,j], at=histogramMids[j], add=TRUE, border="red")
}

# risk CCDF
exceedence <- t(apply(riskResults, 1, function(x) rev(cumsum(rev(x)))))

windows()
plot(c(0.1,0.1),c(1,1), xlim=c(0.3,max(histogramBins)), ylim=c(0,1),
  ylab="Exceedence Probability", xlab="Consequence", axes=FALSE, type="n", log="x")
axis(2)
axis(1, at=c(0.3,histogramBins[2:length(histogramBins)]),
  labels=c(0,round(histogramBins[2:length(histogramBins)],0)))
for (i in 1:N)
{
  lines(c(0.3,histogramBins[2:length(histogramBins)]), c(exceedence[i,],0))
}

windows()
plot(c(0.1,0.1),c(1,1), xlim=c(0.3,max(histogramBins)), ylim=c(1E-3,1),
  ylab="Exceedence Probability", xlab="Consequence", axes=FALSE, type="n",
  log="xy")
axis(2)
axis(1, at=c(0.3,histogramBins[2:length(histogramBins)]),
  labels=c(0,round(histogramBins[2:length(histogramBins)],0)))
for (i in 1:N)
{
  lines(c(0.3,histogramBins[2:length(histogramBins)]), c(exceedence[i,],0))
}

risk <-  riskResults %*% matrix(histogramMids, ncol=1)

windows()
boxplot(risk, ylab="Risk Given Initiation (consequences)", ylim=c(0,15), boxwex=0.2)
points(1, mean(risk), pch=19, col="red")
points(rep(1, N), risk)
axis(1, at=1, label="All Scenarios")

#risk of each scenarios
riskScenarios <- t(apply(riskResultsA, c(1,2), "%*%", histogramMids))

windows()
boxplot(riskScenarios, ylab="Risk Given Initiation (consequences)", ylim=c(0,15),
  axes=FALSE)
points(1:4, apply(riskScenarios, 2, mean), pch=19, col="red")
axis(2)
axis(1, at=1:4, label=paste("Scenario",1:4))


# Weighted Average Consequences
totFreq <- t(apply(riskResultsA, c(1,2), sum))
weightAvgConsequences <- matrix(NA, nrow=N, ncol=4)
for (i in 1:4)
{
  weightAvgConsequences[,i] <- apply(riskResultsA[i,,], 2, "/", totFreq[,i]) %*% histogramMids
}

windows()
boxplot(weightAvgConsequences, ylab="Weighted Average Consequences Given Attack",
  ylim=c(0,25), axes=FALSE)
points(1:4, apply(weightAvgConsequences, 2, mean), pch=19, col="red")
axis(2)
axis(1, at=1:4, label=paste("Scenario",1:4))



