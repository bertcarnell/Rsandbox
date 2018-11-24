## improved boxplot

boxplot.quant <- function(x, quants=c(0.05, 0.25, 0.5, 0.75, 0.95), plotMean=TRUE, na.rm=TRUE, ...)
{
  #x <- cbind(rnorm(100, 1, 4), rnorm(100, 2, 2), rnorm(100, 3, 0.5))
  #quants <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  #na.rm <- TRUE
  
  stopifnot(all(quants <= 1.0))
  quants <- sort(quants)
  bp <- boxplot(x, plot=FALSE, range=0)
  
  if (is.data.frame(x) || is.matrix(x))
  {
    bp$stats <- apply(x, 2, quantile, probs=quants, na.rm=na.rm)
    means <- apply(x, 2, mean, na.rm=na.rm)
  } else if (is.list(x))
  {
    bp$stats <- sapply(x, quantile, probs=quants, na.rm=na.rm, USE.NAMES=FALSE)
    means <- sapply(x, mean, na.rm=na.rm, USE.NAMES=FALSE)
  } else if (is.numeric(x))
  {
    bp$stats <- matrix(quantile(x, probs=quants, na.rm=na.rm), ncol=1)
    means <- mean(x, na.rm=na.rm)
  } else
  {
    stop("x is not recognized")
  }
  
  bxp(bp, ...)

  if (plotMean)
  {
    points(1:length(means), means,pch=19, col="black")
  }
  
  return(list(quants=bp$stats, means=means))
}


x <- cbind(rnorm(100, 1, 4), rnorm(100, 2, 2), rnorm(100, 3, 0.5))
y <- list(rnorm(100, 1, 4), rnorm(100, 2, 2), rnorm(100, 3, 0.5))
z <- rnorm(1000, 3, 4)

boxplot.quant(x)
boxplot.quant(x, boxfill="red", boxwex=0.5)
boxplot.quant(y)
boxplot.quant(z)

  