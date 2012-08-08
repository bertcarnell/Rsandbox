findDiscreteTriangleMode <- function(xDHist, desiredMean)
{
  stopifnot(require(triangle))
  f <- function(m)
  {
    pHist <- dtriangle(xDHist, min(xDHist)-1, max(xDHist)+1, m)
    return((sum(pHist * xDHist) - desiredMean)^2)
  }

  o <- optimize(f, lower=min(xDHist), upper=max(xDHist))
  return(o$minimum)
}

#triangleMode <- findDiscreteTriangleMode(1:8, desiredMean=3)

#mean(sample(1:8, 100000, replace=TRUE, prob=dtriangle(1:8, 0, 9, triangleMode)))
