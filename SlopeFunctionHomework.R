
plotSlopeField <- function(f)
{
  plot(c(-5,5), c(-5,5), type="n", axes=FALSE, main=capture.output(f), xlab="", ylab="")
  X <- expand.grid(seq(-5,5,by=0.5), seq(-5,5,by=0.5))
  
  slope <- f(X[,1], X[,2])
  
  angle <- atan(slope)
  x1 <- X[,1] - 0.25*cos(angle)
  x2 <- X[,1] + 0.25*cos(angle)
  y1 <- X[,2] - 0.25*sin(angle)
  y2 <- X[,2] + 0.25*sin(angle)
  
  for (i in 1:length(x1))
  {
    lines(c(x1[i], x2[i]), c(y1[i], y2[i]), col="red")
  }
  axis(1, pos=0, labels=NA, at=-5:5)
  axis(2, pos=0, labels=NA, at=-5:5)
}

f1 <- function(x,y) 0.5*x+1
plotSlopeField(f1)

f1 <- function(x,y) x-y
plotSlopeField(f1)

f1 <- function(x,y) y
plotSlopeField(f1)

f1 <- function(x,y) -1*x/y
plotSlopeField(f1)

f1 <- function(x,y) x*y
plotSlopeField(f1)

f1 <- function(x,y) 2+x
plotSlopeField(f1)

f1 <- function(x,y) 6/x
plotSlopeField(f1)

f1 <- function(x,y) x+3*y
plotSlopeField(f1)
