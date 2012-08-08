x <- matrix(c(runif(1000, 0, 0.2),
              runif(1000, 0.2, 0.5),
              runif(1000, 0.5, 0.8),
              runif(1000, 0.8, 1)), nrow=1000, ncol=4)
rSums <- rowSums(x)
x <- apply(x, 2, "/", rSums)
stopifnot(all(rowSums(x) - 1 < 1E-9))
p <- apply(x, 2, mean)
windows()
pie(p)
quants <- apply(x, 2, quantile, prob=c(0.05, 0.95))
id <- matrix(rep(LETTERS[1:4], each=3), nrow=3, ncol=4)
stats <- rbind(quants[1,], p, quants[2,])
stopifnot(order(c(stats)) == 1:12)
windows()
pie(p)
pie(c(stats), col=NA)

windows()
ord <- order(p, decreasing=TRUE)
pie(p[ord], border=NA, col=c(colorRampPalette(c("white","red"))(3)[2],
                             colorRampPalette(c("white","blue"))(3)[2],
                             colorRampPalette(c("white","green"))(3)[2],
                             colorRampPalette(c("white","orange"))(3)[2]))
for (i in 1:100)
{
  par(new=TRUE)
  pie(x[i,ord], border=c("white","red","blue","green"), col=NA, labels=NA, radius=0.6)
}

#hsvcol <- rgb2hsv(col2rgb(c("red", "blue", "lightblue")))
#hsvcol[,2] <- c(0,.5,.5)
#hsvcol[,3] <- c(0,.1,.5)
#plot(1:3, 1:3, pch=19, col=c("red", "blue", "lightblue"), cex=5)
#plot(1:3, 1:3, pch=19, col=col2rgb(c("red", "blue", "lightblue")), cex=5)

# well behaved example
windows()
set.seed(1976)
x <- matrix(c(runif(1000, 0.2, 0.3),
              runif(1000, 0.2, 0.3),
              runif(1000, 0.2, 0.3),
              runif(1000, 0.2, 0.3)), nrow=1000, ncol=4)
p <- apply(x, 2, mean)
ord <- order(p, decreasing=TRUE)
# make lighter colors out of mean pie
pie(p[ord], border=NA, col=c(colorRampPalette(c("white","red"))(5)[2],
                             colorRampPalette(c("white","blue"))(5)[2],
                             colorRampPalette(c("white","green"))(5)[2],
                             colorRampPalette(c("white","orange"))(5)[2]))
# draw bright colored lines
for (i in 1:N)
{
  par(new=TRUE)
  pie(x[i,ord], border=c("white","red","blue","green"), col=NA, labels=NA, radius=0.6)
}

# not so well behaved
windows()
require(dirichlet)
set.seed(1976)
N <- 50
k <- 5
p <- c(0.6, 0.2, 0.15, 0.05)
x <- rdirichlet(N, k*p)

p <- apply(x, 2, mean)
ord <- order(p, decreasing=TRUE)
pie(p[ord], border=NA, col=c(colorRampPalette(c("white","red"))(5)[2],
                             colorRampPalette(c("white","blue"))(5)[2],
                             colorRampPalette(c("white","green"))(5)[2],
                             colorRampPalette(c("white","orange"))(5)[2]))
for (i in 1:N)
{
  par(new=TRUE)
  pie(x[i,ord], border=c("white","red","blue","green"), col=NA, labels=NA, radius=0.6)
}

# maybe this is an onion chart?
# atleast it shows that when red is small, others are large...
windows()
N <- 50
set.seed(1976)
x <- rdirichlet(N, k*p)
p <- apply(x, 2, mean)
ord <- order(p, decreasing=TRUE)
roword <- order(x[,ord[1]], decreasing=TRUE)
# get labels right...
pie(p[ord], col=c("red","blue","green", "orange"), radius=1)
for (i in 1:100)
{
  par(new=TRUE)
  pie(x[roword[i],ord], col=c("red","blue","green", "orange"), labels=NA,
    radius=0.3+0.7/N*(N-i+1))
}
# mean pie in the center
par(new=TRUE)
pie(p[ord], col=c("red","blue","green", "orange"), radius=0.3, labels=NA)





pieUncertainty <- function (mat, labels, edges = 200, radius = 0.8, clockwise = FALSE, 
    init.angle = if (clockwise) 90 else 0,  
    col = NULL, border = NULL, lty = NULL, main = NULL, div = 4, ...) 
{
    if (dim(mat)[1]!=3)
        stop("mat must have uncertainty in the rows")
    if (!is.numeric(mat) || any(is.na(mat) | mat < 0)) 
        stop("'mat' values must be positive.")
    if (is.null(labels))
    { 
       labels <- as.character(1:length(dim(mat)[2]))
    } else
    {
      labels <- as.graphicsAnnot(labels)
    }
    np <- dim(mat)[2]
    stopifnot(np == length(col))

    stopifnot(order(c(mat)) == 1:length(c(mat)))

    # x will unroll by columns
    x <- c(0, cumsum(mat)/sum(mat))
    dx <- diff(x)
    # change x so that we have lots of divisions in the uncertainty areas
    # 0 lp1 p1 up1 lp2 p2 up2 lp3 p3 up3
    # 0 0+dx1/div . . lp1 p1 up1 up1+dx3/div
    y <- numeric(1+div*np+np+1+1)
    yposit <- 1
    xposit <- 1
    for (i in 1:np)
    {
      y[yposit] <- x[xposit]
      yposit <- yposit + 1
      for (j in 1:div)
      {
        y[yposit] <- x[xposit] + j*dx[xposit]/div
        yposit <- yposit + 1
      }
	xposit <- xposit + 2
      y[yposit] <- x[xposit]
      yposit <- yposit + 1
	xposit <- xposit + 1
    }
    y[yposit] <- x[xposit]

    dx <- diff(y)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1] > pin[2])
    { 
      xlim <- (pin[1]/pin[2]) * xlim
    } else
    {
      ylim <- (pin[2]/pin[1]) * ylim
    }
    plot.window(xlim, ylim, "", asp = 1)

    if (is.null(col))
    { 
      if (is.null(density))
      {
        col <- c("white", "lightblue", "mistyrose", "lightcyan", 
                "lavender", "cornsilk")
        col <- rep(col, length.out = np)
      } else par("fg")
    }

    newCol <- character(nx)
    colposit <- 1
    for (i in 1:np)
    {
      rampposit <- 2
      if (i-1==0)
      {
        colramp <- colorRampPalette(c(col[np],col[i]))(div+2)
      } else colramp <- colorRampPalette(c(col[i-1],col[i]))(div+2)
      newCol[colposit] <- colramp[rampposit]
      colposit <- colposit+1
      rampposit <- rampposit+1
      for (j in 1:div)
      {
        newCol[colposit] <- colramp[rampposit]
        colposit <- colposit+1
        rampposit <- rampposit+1
      }
      newCol[colposit] <- col[i]
      colposit <- colposit + 1
    }
 
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    twopi <- if (clockwise) -2 * pi else 2 * pi
    t2xy <- function(t) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radius * cos(t2p), y = radius * sin(t2p))
    }
    x <- y
    col <- newCol
    for (i in 1:nx) 
    {
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
        polygon(c(P$x, 0), c(P$y, 0), border = border[i], col = col[i], lty = lty[i])
        P <- t2xy(mean(x[i + 0:1]))
        lab <- as.character(labels[i])
        if (!is.na(lab) && nzchar(lab)) {
            lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
            text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE, 
                adj = ifelse(P$x < 0, 1, 0))#, ...)
        }
    }
    title(main = main, ...)
    invisible(NULL)
}

pieUncertainty(stats, LETTERS[1:4], col =c("red", "green", "blue", "orange"), div=4) 
pieUncertainty(stats, LETTERS[1:4], col =c("red", "green", "blue", "orange"), div=10, border=NA) 

stats <- matrix(c(0.20,0.25,0.3,0.2,0.25,0.3,0.2,0.25,0.3,0.2,0.25,0.3), nrow=3, ncol=4)
pieUncertainty(stats, LETTERS[1:4], col =c("red", "green", "blue", "orange"), div=10, border=NA) 



