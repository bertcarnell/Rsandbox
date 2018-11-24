require(randomForest)
require(RColorBrewer)
require(MASS)

data(iris)
set.seed(71)
iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)

## dirichlet based on confusion matrix
rdirichlet <- function(alpha, N)
{
  samp <- function(a) rgamma(N, a, 1)
  X <- sapply(alpha, samp, USE.NAMES=FALSE)
  norm <- rowSums(X)
  return(X / norm)
}

# want a dirichlet with means equal to the predicted probabilities for an observation
#  and a k value that matchs the error in the confusion matrix
p <- predict(iris.rf, iris[150,], type="prob")
N <- 10000

f <- function(k, N, p, desiredError)
{
  stopifnot(length(p) == length(desiredError))
  X <- rdirichlet(p*k, N)
  r <- t(apply(X, 1, rank))

  # given that we have predicted a max(p), what is error rate for the others
  error <- apply(r, 2, function(y) length(which(y == length(p)))/N)
  return(sum((desiredError-error)^2))
}

set.seed(2001)
optimize(f, lower=0, upper=10, N=N, p=p, 
  desiredError=iris.rf$confusion[,which.max(p)]/sum(iris.rf$confusion[,which.max(p)]), tol=1E-6)
f(1, N=N, p=p, desiredError=iris.rf$confusion[,which.max(p)]/sum(iris.rf$confusion[,which.max(p)]))
f(10, N=N, p=p, desiredError=iris.rf$confusion[,which.max(p)]/sum(iris.rf$confusion[,which.max(p)]))
f(100, N=N, p=p, desiredError=iris.rf$confusion[,which.max(p)]/sum(iris.rf$confusion[,which.max(p)]))
f(1000, N=N, p=p, desiredError=iris.rf$confusion[,which.max(p)]/sum(iris.rf$confusion[,which.max(p)]))

f <- function(x, confusionrow)
{
  # x is a dirichlet sample
  # confu is a row of the confusion matrix
  k <- ncol(x)
  n <- nrow(x)
  stopifnot(length(confusionrow) == k+1)
  errors <- numeric(k)
  errorrate <- confusionrow/
  r <- apply(X, 1, rank)
  for (i in 1:k)
  {
    # for the first row of the confusion matrix, make a dirichlet with the right error rate
    errorrate <- confu[i,1:k]
  }
  
}

if (all(iris.rf[,4] == 0))
{
} else 
{
}


## Look at variable importance:
round(importance(iris.rf), 2)
windows()
varImpPlot(iris.rf)

## Do MDS on 1 - proximity:
windows()
iris.mds <- cmdscale(1 - iris.rf$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(iris[,1:4], iris.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(iris$Species)],
      main="Iris Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(iris.mds$GOF)

windows()
output <- MDSplot(iris.rf, iris$Species, k=2, pch=as.numeric(iris$Species))
legend("topleft", legend=as.character(unique(iris$Species)), 
  pch=as.numeric(unique(iris$Species)),
  col=brewer.pal(3, "Set1"))

windows()
output <- MDSplot(iris.rf, iris$Species, k=3, pch=as.numeric(iris$Species))
legend("topleft", legend=as.character(unique(iris$Species)), 
  pch=as.numeric(unique(iris$Species)),
  col=brewer.pal(3, "Set1"))



# add a new point not in the dataset, try to print it on the plot
windows()
boxplot(iris)

# can't because MDS plot needs the proximity matrix which is in the Random forest function

# try to fit it as part of the Random Forest

iris.outlier <- cbind(rbind(iris[,1:4], c(9,9,9,9)), c(as.character(iris$Species), "new"))
names(iris.outlier)[5] <- "Species"

iris.rf.outlier <- randomForest(Species ~ ., data=iris.outlier, importance=TRUE,
                        proximity=TRUE)
output <- MDSplot(iris.rf.outlier, iris.outlier$Species, k=3, 
  pch=as.numeric(iris.outlier$Species))
legend("topleft", legend=as.character(unique(iris.outlier$Species)), 
  pch=as.numeric(unique(iris.outlier$Species)),
  col=brewer.pal(3, "Set1"))
o <- outlier(iris.rf.outlier$proximity)
summary(o)
windows()
boxplot(o)

# won't work since there is only one observation in the outlier category
#o <- outlier(iris.rf.outlier)
#o <- outlier(iris.rf.outlier, cls=iris.outlier$Species)
#o <- outlier(iris.rf.outlier$proximity, cls=iris.outlier$Species)

iris.outlier <- cbind(rbind(iris[,1:4], c(9,9,9,9)), 
  c(as.character(iris$Species), "setosa"))
names(iris.outlier)[5] <- "Species"

iris.rf.outlier <- randomForest(Species ~ ., data=iris.outlier, importance=TRUE,
                        proximity=TRUE)
output <- MDSplot(iris.rf.outlier, iris.outlier$Species, k=3, 
  pch=as.numeric(iris.outlier$Species))
legend("topleft", legend=as.character(unique(iris.outlier$Species)), 
  pch=as.numeric(unique(iris.outlier$Species)),
  col=brewer.pal(3, "Set1"))
plot(outlier(iris.rf.outlier), 
  col=brewer.pal(3, "Set1")[as.numeric(iris$Species)])

set.seed(1976)  
iris.outlier <- cbind(rbind(iris[,1:4], c(9,9,9,9)), 
  c(as.character(iris$Species), "setosa"))
names(iris.outlier)[5] <- "Species"
for (i in 1:3)
{
  print(i)
  
  iris.outlier$Species[151] <- levels(iris.outlier$Species)[i]
  iris.rf.outlier <- randomForest(Species ~ ., data=iris.outlier, 
    importance=TRUE, proximity=TRUE)
  o <- outlier(iris.rf.outlier)
  print(o[151])

  ind <- which(as.numeric(iris.outlier$Species) == i)

  d <- (iris.rf.outlier$proximity)^2
  diag(d) <- 0
  m <- length(diag(d)) / apply(d, 1, sum)
  print((0.6745*(m[ind]-median(m[ind]))/median(abs(m[ind]-median(m[ind]))))[length(ind)])

  d <- (iris.rf.outlier$proximity)^2
  m <- length(diag(d)) / apply(d, 1, sum)
  print((0.6745*(m[ind]-median(m[ind]))/median(abs(m[ind]-median(m[ind]))))[length(ind)])

  m <- length(ind) / apply(d, 1, sum)
  print((0.6745*(m[ind]-median(m[ind]))/median(abs(m[ind]-median(m[ind]))))[length(ind)])

  m <- 1 / apply(d, 1, sum)
  print((0.6745*(m[ind]-median(m[ind]))/median(abs(m[ind]-median(m[ind]))))[length(ind)])

  m <- length(diag(d)) / apply(d, 1, sum)
  print((0.6745*(m[ind]-median(m[ind]))/median(abs(m-median(m))))[length(ind)])

  d <- (iris.rf.outlier$proximity)^2
  diag(d) <- 0
  m <- length(diag(d)) / apply(d, 1, sum)
  print(((m[ind]-median(m[ind]))/median(abs(m[ind]-median(m[ind]))))[length(ind)])
}

# MAD per class
d <- (iris.rf.outlier$proximity)^2
diag(d) <- 0
m <- length(diag(d)) / apply(d, 1, sum)
by(m, iris.outlier$Species, function(m) 0.6745*(m-median(m))/median(abs(m-median(m))))


# try to fit it into each existing group and look for outlier indicator in all groups...

        

X <- as.matrix(iris[, -5])
X <- X[-143,]
dim(X)
X.dist <- dist(X)
str(X.dist)

X.mds <- isoMDS(X.dist)
plot(X.mds$points, type = "n")
points(X.mds$points, pch = as.numeric(unique(iris$Species)), 
  col=as.numeric(unique(iris$Species)))

X.sh <- Shepard(X.dist, X.mds$points)
plot(X.sh, pch = ".")
lines(X.sh$x, X.sh$yf, type = "S")

######

iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE, ntree=3000, mtry=3)
iris.rf
noprior <- predict(iris.rf, type="prob")

iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE, ntree=3000, mtry=3, 
                        classwt=c(0.1,0.8,0.1))
iris.rf
withprior <- predict(iris.rf, type="prob")

all(noprior[,2] <= withprior[,2]) # FALSE
cbind(noprior[,2], withprior[,2], iris$Species)

####

iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE, ntree=3000, mtry=3)
p <- predict(iris.rf, newdata=iris[134,-5], type="prob")

p[1] - iris.rf$confusion[2,1]*p[1] - iris.rf$confusion[3,1]*p[1]
p[2] + iris.rf$confusion[2,1]*p[1]
p[3] + iris.rf$confusion[3,1]*p[1]

p
p[2] - iris.rf$confusion[1,2]/sum(iris.rf$confusion[,2])*p[2] - iris.rf$confusion[3,2]/sum(iris.rf$confusion[,2])*p[2]
p[1] + iris.rf$confusion[1,2]/sum(iris.rf$confusion[,2])*p[2]
p[3] + iris.rf$confusion[3,2]/sum(iris.rf$confusion[,2])*p[2]
p
p[2] - iris.rf$confusion[1,2]/sum(iris.rf$confusion[,2])*p[2] - iris.rf$confusion[3,2]/sum(iris.rf$confusion[,2])*p[2]
p[1] + iris.rf$confusion[1,2]/sum(iris.rf$confusion[,2])*p[2]
p[3] + iris.rf$confusion[3,2]/sum(iris.rf$confusion[,2])*p[2]



p[3] - iris.rf$confusion[1,3]*p[3] - iris.rf$confusion[2,3]*p[3]