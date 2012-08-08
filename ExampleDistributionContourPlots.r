# code to plot 2 dimensional contour plots of distributions for comparison, overlayed

require(ks)
require(RColorBrewer)
require(mvtnorm)

x <- rmvnorm(100, c(1,2), matrix(c(1.1,1,1,1.2), nrow=2))
Hx <- Hpi(x)
fhatx <- kde(x=x, H=Hx, xmin=c(-6,-6), xmax=c(6,6))

y <- rmvnorm(100, c(-1,0), 0.5*diag(2))
Hy <- Hpi(y)
fhaty <- kde(x=y, H=Hy, xmin=c(-6,-6), xmax=c(6,6))

levx <- contourLevels(fhatx, prob=c(0.05, 0.25, 0.5, 0.75, 0.95, 0.999))
levy <- contourLevels(fhaty, prob=c(0.05, 0.25, 0.5, 0.75, 0.95, 0.999))

mypalettegreen <- brewer.pal(length(levx),"Greens")
mypalettered <- brewer.pal(length(levy),"Reds")
mypalettegreen <- apply(col2rgb(mypalettegreen), 2, 
  function(x) rgb(red=x[1], green=x[2], blue=x[3], alpha=220, maxColorValue=255))
mypalettered <- apply(col2rgb(mypalettered), 2, 
  function(x) rgb(red=x[1], green=x[2], blue=x[3], alpha=255/2, maxColorValue=255))
mypalettegreen[1] <- "#FFFFFF00"
mypalettered[1] <- "#FFFFFF00"

windows()
plot(fhatx, display="filled.contour2", abs.cont=levx, col=mypalettegreen)
plot(fhaty, display="filled.contour2", abs.cont=levy, add=TRUE, col=mypalettered)

plotKde2D <- function (fhat, display = "slice", cont = c(25, 50, 75), abs.cont, 
    approx.cont = FALSE, xlab, ylab, zlab = "Density function", 
    cex = 1, pch = 1, labcex, add = FALSE, drawpoints = FALSE, 
    drawlabels = TRUE, theta = -30, phi = 40, d = 4, ptcol = "blue", 
    col, lwd = 1, contourLabels=NULL, ...) 
{
    disp1 <- substr(display, 1, 1)
    if (!is.list(fhat$eval.points)) 
        stop("Need a grid of density estimates")
    if (missing(xlab)) 
        xlab <- fhat$names[1]
    if (missing(ylab)) 
        ylab <- fhat$names[2]
    if (missing(labcex)) 
        labcex <- 1
    if (missing(approx.cont)) 
        approx.cont <- (nrow(fhat$x) > 2000)
    if (disp1 == "p") 
        plotret <- persp(fhat$eval.points[[1]], fhat$eval.points[[2]], 
            fhat$estimate, theta = theta, phi = phi, d = d, xlab = xlab, 
            ylab = ylab, zlab = zlab, ...)
    else if (disp1 == "s") {
        if (!add) 
            plot(fhat$x[, 1], fhat$x[, 2], type = "n", xlab = xlab, 
                ylab = ylab, ...)
        if (missing(abs.cont)) {
            if (!is.null(fhat$cont)) {
                cont.ind <- rep(FALSE, length(fhat$cont))
                for (j in 1:length(cont)) cont.ind[which(cont[j] == 
                  as.numeric(unlist(strsplit(names(fhat$cont), 
                    "%"))))] <- TRUE
                if (all(!cont.ind)) 
                  hts <- contourLevels(fhat, prob = (100 - cont)/100, 
                    approx = approx.cont)
                else hts <- fhat$cont[cont.ind]
            }
            else hts <- contourLevels(fhat, prob = (100 - cont)/100, 
                approx = approx.cont)
        }
        else hts <- abs.cont
        hts <- sort(hts)
        if (missing(col)) 
            col <- 1
        if (length(col) < length(hts)) 
            col <- rep(col, times = length(hts))
        for (i in 1:length(hts)) {
            if (missing(abs.cont)) 
                scale <- cont[i]/hts[i]
            else scale <- 1
            if (hts[i] > 0) 
                contour(fhat$eval.points[[1]], fhat$eval.points[[2]], 
                  fhat$estimate * scale, level = hts[i] * scale, 
                  add = TRUE, drawlabels = drawlabels, labcex = labcex, 
                  col = col[i], lwd = lwd, labels=contourLabels[i], ...)
        }
        if (drawpoints) 
            points(fhat$x[, 1], fhat$x[, 2], col = ptcol, cex = cex, 
                pch = pch)
    }
    else if (disp1 == "i") {
        image(fhat$eval.points[[1]], fhat$eval.points[[2]], fhat$estimate, 
            xlab = xlab, ylab = ylab, add = add, ...)
        box()
    }
    else if (disp1 == "f") {
        if (display == "filled.contour2") {
            if (missing(abs.cont)) {
                if (!is.null(fhat$cont)) {
                  cont.ind <- rep(FALSE, length(fhat$cont))
                  for (j in 1:length(cont)) cont.ind[which(cont[j] == 
                    as.numeric(unlist(strsplit(names(fhat$cont), 
                      "%"))))] <- TRUE
                  if (all(!cont.ind)) 
                    hts <- contourLevels(fhat, prob = (100 - 
                      cont)/100, approx = approx.cont)
                  else hts <- fhat$cont[cont.ind]
                }
                else hts <- contourLevels(fhat, prob = (100 - 
                  cont)/100, approx = approx.cont)
            }
            else hts <- abs.cont
            hts <- sort(hts)
            if (missing(col)) 
                col <- c("transparent", rev(heat.colors(length(hts))))
            clev <- c(-0.01 * max(abs(fhat$estimate)), hts, max(c(fhat$estimate, 
                hts)) + 0.01 * max(abs(fhat$estimate)))
            image(fhat$eval.points[[1]], fhat$eval.points[[2]], 
                fhat$estimate, xlab = xlab, ylab = ylab, add = add, 
                col = col[1:(length(hts) + 1)], breaks = clev, 
                ...)
            for (i in 1:length(hts)) contour(fhat$eval.points[[1]], 
                fhat$eval.points[[2]], fhat$estimate, level = hts[i], 
                add = TRUE, drawlabels = FALSE, col = col[i + 
                  1], lwd = 7, labels=contourLabels[i])
            if (!missing(lwd)) {
                for (i in 1:length(hts)) {
                  if (missing(abs.cont)) 
                    scale <- cont[i]/hts[i]
                  else scale <- 1
                  contour(fhat$eval.points[[1]], fhat$eval.points[[2]], 
                    fhat$estimate * scale, level = hts[i] * scale, 
                    add = TRUE, drawlabels = drawlabels, col = 1, 
                    labcex = labcex, lwd = lwd, labels=contourLabels[i], ...)
                }
            }
        }
        else filled.contour(fhat$eval.points[[1]], fhat$eval.points[[2]], 
            fhat$estimate, xlab = xlab, ylab = ylab, ...)
    }
    if (disp1 == "p") 
        invisible(plotret)
    else invisible()
}

contourProbs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
contourLabs <- paste(contourProbs*100, "%", sep="") 
levx <- contourLevels(fhatx, prob=contourProbs)
levy <- contourLevels(fhaty, prob=contourProbs)

mypalettegreen <- brewer.pal(length(levx),"Greens")
mypalettered <- brewer.pal(length(levy),"Reds")

windows()
plotKde2D(fhatx, display="slice", abs.cont=levx, col="green", 
  lwd=3, drawlabels=TRUE, contourLabels=contourLabs,
  xlim=c(-3,5), ylim=c(-3,5))
plotKde2D(fhaty, display="slice", abs.cont=levy, add=TRUE, col="red",
  drawlabels=TRUE, lwd=3, contourLabels=contourLabs)

windows()
plotKde2D(fhatx, display="slice", abs.cont=levx, col=mypalettegreen, 
  lwd=3, drawlabels=TRUE, contourLabels=rev(contourLabs),
  xlim=c(-3,5), ylim=c(-3,5))
plotKde2D(fhaty, display="slice", abs.cont=levy, add=TRUE, col=mypalettered,
  drawlabels=TRUE, lwd=3, contourLabels=rev(contourLabs))

windows()
plotKde2D(fhatx, display="slice", abs.cont=levx, col=mypalettegreen, 
  lwd=3, drawlabels=TRUE, contourLabels=contourLabs,
  xlim=c(-3,5), ylim=c(-3,5), drawpoints=TRUE, cex=0.5, ptcol="green")
plotKde2D(fhaty, display="slice", abs.cont=levy, add=TRUE, col=mypalettered,
  drawlabels=TRUE, lwd=3, contourLabels=contourLabs, 
  drawpoints=TRUE, cex=0.5, ptcol="red")

#source("http://bioconductor.org/biocLite.R")
#biocLite()

require("geneplotter")
require("RColorBrewer")

smoothScatter(x)
smoothScatter(x, nrpoints=0)
smoothScatter(x, nrpoints=100)
smoothScatter(x, nrpoints=Inf, bandwidth=0.5)
plot(x, col=densCols(x), pch=20)



