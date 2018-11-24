# develop nodes

# read XSDL graph
f <- file.path("C:","Users","Rob","Desktop","Conjoint.xdsl")

require(XML)

X <- xmlParse(f)
nodeset <- getNodeSet(X, "/smile/nodes/cpt")
nodeids <- sapply(nodeset, function(n) xmlGetAttr(n, "id"))

nodeStates <- sapply(nodeset, function(n) getNodeSet(n, "state"))
nodeStates <- sapply(nodeStates, function(n) xmlApply(n, function(z) xmlGetAttr(z, "id")))
nodeStates <- sapply(nodeStates, unlist)

nodeParents <- sapply(nodeset, function(n) getNodeSet(n, "parents"))
nodeParents <- sapply(nodeParents, function(n) xmlApply(n, function(z) xmlValue(z)))
nodeParents <- sapply(nodeParents, unlist)
nodeParents <- sapply(nodeParents, function(n) if(!is.null(n)) strsplit(n, split=" "))
nodeParents <- sapply(nodeParents, unlist)

nodeProbs <- sapply(nodeset, function(n) getNodeSet(n, "probabilities"))
nodeProbs <- sapply(nodeProbs, function(n) xmlApply(n, function(z) xmlValue(z)))
nodeProbs <- sapply(nodeProbs, unlist)
nodeProbs <- sapply(nodeProbs, strsplit, split=" ")
nodeProbs <- sapply(nodeProbs, unlist)
nodeProbs <- sapply(nodeProbs, as.numeric)

names(nodeStates) <- nodeids
names(nodeParents) <- nodeids
names(nodeProbs) <- nodeids

# enumerate paths of size N
NODES <- length(nodeids)

for (i in 1:NODES)
{
  print(paste("checking: ", nodeids[[i]]))
  if (is.null(nodeParents[[i]]))
  {
    stopifnot(length(nodeStates[[i]]) == length(nodeProbs[[i]]))
  } else {
    stopifnot(length(nodeStates[[i]]) * prod(sapply(nodeParents[[i]], function(n) length(nodeStates[[n]]))) == length(nodeProbs[[i]]))
  }
}

nodeProbArrays <- vector("list", length=NODES)
nodeDims <- vector("list", length=NODES)
for (i in 1:NODES)
{
  print(paste("checking: ", nodeids[[i]]))
  if (is.null(nodeParents[[i]]))
  {
    nodeProbArrays[[i]] <- nodeProbs[[i]]
    nodeDims[[i]] <- length(nodeProbs[[i]])
  } else {
    nodeDims[[i]] <- c(length(nodeStates[[i]]), sapply(nodeParents[[i]], function(n) length(nodeStates[[n]])))
    nodeProbArrays[[i]] <- array(nodeProbs[[i]], dim=nodeDims[[i]])
  }
}

dualScenarios <- character()
for (i in 1:(NODES-1))
{
  for (j in 1:length(nodeStates[[i]]))
  {
    for (k in (i+1):NODES)
    {
      for (m in 1:length(nodeStates[[k]]))
      {
        dualScenarios <- c(dualScenarios, paste(nodeStates[[i]][j], nodeStates[[k]][m], sep="_"))
      }
    }
  }
}

#nodeProbArrays[[1]] %*% t(nodeProbArrays[[2]])

#specify the model
require(gRain)
LEVELS <- c("ACQ","ACL","MAT","AMT","INT","SF")
ACQ_BRANCHES <- paste("ACQ", 1:4, sep="_")
ACL_BRANCHES <- c(paste("ACL", 0:3, sep="_"), "NA")
MAT_BRANCHES <- c(paste("MAT", 1:3, sep="_"), "NA")
AMT_BRANCHES <- c(paste("AMT", 1:4, sep="_"), "NA")

vals <- rep(1,length(ACQ_BRANCHES))
acqTable <- cptable(~ACQ, values=vals, levels=ACQ_BRANCHES)
vals <- matrix(c(rep(1,4),0,rep(1,4),0,rep(1,4),0,rep(0,4), 4), nrow=length(ACL_BRANCHES), ncol=length(ACQ_BRANCHES))
stopifnot(all(apply(vals, 2, sum) == 4))
aclTable <- cptable(~ACL|ACQ, values=vals, levels=ACL_BRANCHES)
vals <- array(c(rep(1,3), 0, rep(0,3), 1, rep(0, 3), 1, rep(0,3), 1,
                rep(1,3), 0, rep(0,3), 1, rep(0, 3), 1, rep(0,3), 1,
                rep(1,3), 0, rep(0,3), 1, rep(0, 3), 1, rep(0,3), 1,
                rep(1,3), 0, rep(0,3), 1, rep(0, 3), 1, rep(0,3), 1,
                rep(0,3), 1, rep(0,3), 1, rep(0, 3), 1, rep(0,3), 1), dim=c(length(MAT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES)))
matTable <- cptable(~MAT|ACQ:ACL, values=vals, levels=MAT_BRANCHES)
amtTable <- cptable(~AMT|ACQ:ACL, values=vals, levels=MAT_BRANCHES)

################################################################################

rm(list=ls())

#source("http://bioconductor.org/biocLite.R"); biocLite(c("graph","RBGL","Rgraphviz"))
require(XML)
require(gRain)
require(compiler)
require(lhs)
require(minpack.lm)
#install.packages('ppso','http://www.rforge.net/')
require(ppso)
require(parallel)

# read XSDL graph
f <- file.path("C:","Users","Rob","Desktop","Conjoint", "Conjoint.xdsl")

X <- xmlParse(f)
nodeset <- getNodeSet(X, "/smile/nodes/cpt")
nodeids <- sapply(nodeset, function(n) xmlGetAttr(n, "id"))

nodeStates <- sapply(nodeset, function(n) getNodeSet(n, "state"))
nodeStates <- sapply(nodeStates, function(n) xmlApply(n, function(z) xmlGetAttr(z, "id")))
nodeStates <- sapply(nodeStates, unlist)

nodeParents <- sapply(nodeset, function(n) getNodeSet(n, "parents"))
nodeParents <- sapply(nodeParents, function(n) xmlApply(n, function(z) xmlValue(z)))
nodeParents <- sapply(nodeParents, unlist)
nodeParents <- sapply(nodeParents, function(n) if(!is.null(n)) strsplit(n, split=" "))
nodeParents <- sapply(nodeParents, unlist)

nodeProbs <- sapply(nodeset, function(n) getNodeSet(n, "probabilities"))
nodeProbs <- sapply(nodeProbs, function(n) xmlApply(n, function(z) xmlValue(z)))
nodeProbs <- sapply(nodeProbs, unlist)
nodeProbs <- sapply(nodeProbs, strsplit, split=" ")
nodeProbs <- sapply(nodeProbs, unlist)
nodeProbs <- sapply(nodeProbs, as.numeric)

names(nodeStates) <- nodeids
names(nodeParents) <- nodeids
names(nodeProbs) <- nodeids

# enumerate paths of size N
NODES <- length(nodeids)

for (i in 1:NODES)
{
  print(paste("checking: ", nodeids[[i]]))
  if (is.null(nodeParents[[i]]))
  {
    stopifnot(length(nodeStates[[i]]) == length(nodeProbs[[i]]))
  } else {
    stopifnot(length(nodeStates[[i]]) * prod(sapply(nodeParents[[i]], function(n) length(nodeStates[[n]]))) == length(nodeProbs[[i]]))
  }
}

dualScenarios <- list()
count <- 1
for (i in 1:(NODES-1))
{
  for (j in 1:length(nodeStates[[i]]))
  {
    for (k in (i+1):NODES)
    {
      for (m in 1:length(nodeStates[[k]]))
      {
        dualScenarios[[count]] <- c(nodeStates[[i]][j], nodeStates[[k]][m], nodeids[i], nodeids[k])
        count <- count + 1
      }
    }
  }
}

dualScenarioPairs <- combn(dualScenarios, 2)

# create statements of the form
# A1B1_A2B3 <- log(AB["A1","B1"] / AB["A2","B3"]) / log(b)

probStatements <- character(ncol(dualScenarioPairs))
for (j in 1:ncol(dualScenarioPairs))
{
  probStatements[j] <- paste(paste(dualScenarioPairs[1,j][[1]][1:2], collapse=""), "_", 
                             paste(dualScenarioPairs[2,j][[1]][1:2], collapse=""),
                             " <- log(", 
                             paste(dualScenarioPairs[1,j][[1]][3:4], collapse=""), "[\"", 
                             dualScenarioPairs[1,j][[1]][1], "\",\"", dualScenarioPairs[1,j][[1]][2], "\"]/",
                             paste(dualScenarioPairs[2,j][[1]][3:4], collapse=""), "[\"",
                             dualScenarioPairs[2,j][[1]][1], "\",\"", dualScenarioPairs[2,j][[1]][2], 
                             "\"])/log(b)",
                             sep="")
}

# pick a subset of the columns to use
ind <- sample(1:ncol(dualScenarioPairs), size=200, replace=FALSE)

# column meanings
#sapply(probStatements, substring, first=1, last=9, USE.NAMES=FALSE)[ind]

nPeople <- 9
dat <- matrix(sample(-2:2, size=1, replace=TRUE), nrow=length(ind), ncol=nPeople)

f <- function(x, dat)
{
  #x <- c(rep(0.5,23),rep(5,nPeople))
  resultmat <- dat
  
  xa <- c(x[1], 1-x[1])
  xab <- c(x[2], x[3], x[4]*(1-x[2]), x[5]*(1-x[3]), 1-x[4]*(1-x[2])-x[2], 1-x[5]*(1-x[3])-x[3])
  xabc <- numeric(2*3*4)
  xabc[1:6] <- x[6:11] # c1
  xabc[7:12] <- x[12:17]*(1-x[6:11]) #c2
  xabc[13:18] <- x[18:23]*(1-xabc[7:12]-xabc[1:6]) #C3
  xabc[19:24] <- 1-xabc[1:6]-xabc[7:12]-xabc[13:18]
  
  Atable <- cptable(~A, values=xa,levels=c("A1","A2"))
  Btable <- cptable(~B|A, values=xab, levels=c("B1","B2","B3"))
  Ctable <- cptable(~C|A:B, values=xabc, levels=c("C1","C2","C3","C4"))
  plist <- compileCPT(list(Atable, Btable, Ctable))
  net1 <- grain(plist)
  AB <- querygrain(net1, nodes=c("A","B"), type="joint")
  AC <- querygrain(net1, nodes=c("A","C"), type="joint")
  BC <- querygrain(net1, nodes=c("B","C"), type="joint")
  
  # evidence from experts
  for (i in 1:nPeople)
  {
    b <- x[23+i]
    temp <- sapply(probStatements[ind], function(x) eval(parse(text=x)))
    temp2 <- (dat[,i] - temp)
    resultmat[,i] <- temp2*temp2
  }
  return(sum(resultmat))
}
fc <- cmpfun(f)

backTransform <- function(x)
{
  res <- list()
  res[[1]] <- c(x[1], 1-x[1])
  res[[2]] <- matrix(c(x[2], x[3], x[4]*(1-x[2]), x[5]*(1-x[3]), 1-x[4]*(1-x[2])-x[2], 1-x[5]*(1-x[3])-x[3]),
                     nrow=2, ncol=3)
  xabc <- numeric(2*3*4)
  xabc[1:6] <- x[6:11] # c1
  xabc[7:12] <- x[12:17]*(1-x[6:11]) #c2
  xabc[13:18] <- x[18:23]*(1-xabc[7:12]-xabc[1:6]) #C3
  xabc[19:24] <- 1-xabc[1:6]-xabc[7:12]-xabc[13:18]
  res[[3]] <- array(xabc, dim=c(2,3,4))
  return(res)
}

set.seed(1976)
Y <- randomLHS(1000, 32)
Y[,24:32] <- 2 + Y[,24:32]*(10-2)
#system.time({
#Yres <- apply(Y, 1, fc, dat=dat)
#})
#user  system elapsed for N=100
#34.65    0.00   34.85 

cl <- makeCluster(6)
dummy <- clusterEvalQ(cl, {require(gRain)})
clusterExport(cl, c("nPeople", "probStatements","ind"))
system.time({
Yres <- parRapply(cl, Y, fc, dat=dat)
})
#   user  system elapsed for N=100
# 0.00    0.00   25.28 
stopCluster(cl)

system.time({
op2 <- optim(par=Y[which.min(Yres),], fn=fc, dat=dat,
            method="L-BFGS-B",
            lower=c(rep(1E-12, 23), rep(2,nPeople)),
            upper=c(rep(0.9999999, 23), rep(10, nPeople)),
            control=list(maxit=100))
})
#   user  system elapsed 
#3345.69    0.45 3354.85

backTransform(op2$par)
op2$par[24:32]

sum(backTransform(op2$par)[[1]])
apply(backTransform(op2$par)[[2]], 1, sum)
apply(backTransform(op2$par)[[3]], c(1,2), sum)

require(BB)
system.time({
op3 <- spg(par=Y[which.min(Yres),], fc, dat=dat,
        lower=c(rep(1E-12, 23), rep(2,nPeople)),
        upper=c(rep(0.9999999, 23), rep(10, nPeople)),
        control=list(maxit=100))
})

backTransform(op2$par)
op2$par[24:32]



fresid <- function(x, dat)
{
  #x <- c(1,99,1:6,1:24,rep(5,nPeople))
  resultmat <- dat

  xa <- c(x[1], 1-x[1])
  xab <- c(x[2], x[3], x[4]*(1-x[2]), x[5]*(1-x[3]), 1-x[4]*(1-x[2])-x[2], 1-x[5]*(1-x[3])-x[3])
  xabc <- numeric(2*3*4)
  xabc[1:6] <- x[6:11] # c1
  xabc[7:12] <- x[12:17]*(1-x[6:11]) #c2
  xabc[13:18] <- x[18:23]*(1-xabc[7:12]-xabc[1:6]) #C3
  xabc[19:24] <- 1-xabc[1:6]-xabc[7:12]-xabc[13:18]
  
  Atable <- cptable(~A, values=xa,levels=c("A1","A2"))
  Btable <- cptable(~B|A, values=xab, levels=c("B1","B2","B3"))
  Ctable <- cptable(~C|A:B, values=xabc, levels=c("C1","C2","C3","C4"))
  plist <- compileCPT(list(Atable, Btable, Ctable))
  net1 <- grain(plist)
  AB <- querygrain(net1, nodes=c("A","B"), type="joint")
  AC <- querygrain(net1, nodes=c("A","C"), type="joint")
  BC <- querygrain(net1, nodes=c("B","C"), type="joint")
  
  # evidence from experts
  for (i in 1:nPeople)
  {
    b <- x[23+i]
    temp <- sapply(probStatements[ind], function(x) eval(parse(text=x)))
    resultmat[,i] <- (dat[,i] - temp)
  }
  return(c(resultmat))
}

fresidc <- cmpfun(fresid)

nlsres <- nls.lm(par=Y[which.min(Yres),], 
                 lower=c(rep(1E-12, 23), rep(2,nPeople)),
                 upper=c(rep(0.9999999, 23), rep(10, nPeople)),
                 fresidc, dat=dat)

backTransform(coef(nlsres))
coef(nlsres)[24:32]

sum(backTransform(coef(nlsres))[[1]])
apply(backTransform(coef(nlsres))[[2]], 1, sum)
apply(backTransform(coef(nlsres))[[3]], c(1,2), sum)

op2$par[1]
nlsres$par[1]
op2$par[2:4]
nlsres$par[2:4]



