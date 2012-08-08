# explore the difference between a probit model, a linear no-threshold model,
# and a poisson model for dose response of organisms when the ID50
# (infectious dose for 50% probability of infection) is less than one organism

probit <- function(ID50, PS, x)
{
  pnorm(-PS*log10(ID50)+PS*log10(x))
}

poisson_probit <- function(exposure, ID50, PS)
{
  sum_index <- 100
  ind <- which(exposure > sum_index/2)
  dose <- 1:sum_index
  p_illGivenDoseAndExposure <- probit(ID50, PS, dose)
  p_doseGivenExposure <- dpois(dose, exposure)
  res <- sum(p_illGivenDoseAndExposure*p_doseGivenExposure)
  res[ind] <- NA
  res
}


poisson_probit(0.23, 10, 1)

make_graph <- function(ID50, PS)
{
  x <- 10^seq(-2, 3, length=1000)
  y1 <- probit(ID50, PS, x)
  y2 <- sapply(x, poisson_probit, ID50=ID50, PS=PS)
  y3 <- ifelse(x<1, x*probit(ID50, PS, 1), NA)
  

  windows()
  plot(x, y1, col="red", type="l", log="x", main=paste("ID50=",ID50," PS=",PS,sep=""))
  abline(v=10^((qnorm(0.05)+PS*log10(ID50))/PS), h=0.05, lty=2)
  abline(v=1, lty=1)
  points(x, y2, col="blue", type="l")
  points(x, y3, col="green", type="l")
  #ind <- which(abs(y1-y2) < 1E-3)
  #points(x[ind], y1[ind], pch=19, col="black")
  legend("topleft", legend=c("probit", "poisson probit", "linear no threshold"),
    lty=1, col=c("red","blue","green"), bg="white")
}

make_graph(1, 1)
make_graph(1, 2)
make_graph(10, 1)
make_graph(10, 2)
make_graph(100, 1)


