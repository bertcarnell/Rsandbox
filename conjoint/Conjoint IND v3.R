rm(list=ls())

# execute this line of code the first time to install the necessary packages from BioConductor
#source("http://bioconductor.org/biocLite.R"); biocLite(c("graph","RBGL","Rgraphviz"))
# these packages should be installed from CRAN
require(XML)
require(gRain)
require(compiler)
require(lhs)
require(minpack.lm)
require(parallel)

################################################################################

# specify the branch names
LEVELS <- c("ACQ","ACL","MAT","AMT","INT","SF")
NODES <- length(LEVELS)
ACQ_BRANCHES <- paste("ACQ", 1:4, sep="")
ACL_BRANCHES <- c(paste("ACL", 0:3, sep=""), "NA")
MAT_BRANCHES <- c(paste("MAT", 1:3, sep=""), "NA")
AMT_BRANCHES <- c(paste("AMT", 1:4, sep=""), "NA")
INT_BRANCHES <- c("Y","N","NA")
SF_BRANCHES <- c("Y","N","NA")

# number of elicitees
nPeople <- 9

# specify the number of independent probabilities and the nodes they come from
pnames <- c(rep("ACQ", 3), rep("ACL", 9), rep("MAT", 6), rep("AMT", 27), rep("INT", 9), rep("SF", 3))
NINDEP <- 57
stopifnot(length(pnames) == NINDEP)

# number of questions that will be asked of elicitees
NQUESTIONS <- 200
# the logarithm base function for each elicitee
MINLOGBASE <- 2
MAXLOGBASE <- 10

# the number of cores to use for parallel computation
NCORES <- 4

# the number of design points to use in the response surface
NDESIGNPOINTS <- 100

################################################################################

# functions to translate independent parameters into correlated probabilities
fa <- function(x) c(x, 1-x, 0)
fab <- function(x,y) c(x, y*(1-x), 1-x-y*(1-x), 0)
fabc <- function(x,y,z) c(x, y*(1-x), z*(1-x-y*(1-x)), 1-x-y*(1-x)-z*(1-x-y*(1-x)), 0)
fabc_nozero <- function(x,y,z) c(x, y*(1-x), z*(1-x-y*(1-x)), 1-x-y*(1-x)-z*(1-x-y*(1-x)))

fa_c <- cmpfun(fa)
fab_c <- cmpfun(fab)
fabc_c <- cmpfun(fabc)
fabc_nozero_c <- cmpfun(fabc_nozero)

stopifnot(all(fa_c(0.5) == c(0.5, 0.5, 0)))
stopifnot(all(fab_c(0.5,0.5) == c(0.5, 0.25, 0.25, 0)))
stopifnot(all(fabc_c(0.5, 0.5, 0.5) == c(0.5, 0.25, 0.125, 0.125, 0)))
stopifnot(all(fabc_nozero_c(0.5, 0.5, 0.5) == c(0.5, 0.25, 0.125, 0.125)))

#  functions to create the arrays of probabilities at each node
createArrayAcl <- function(x)
{
  vals <- array(0, dim=c(length(ACL_BRANCHES), length(ACQ_BRANCHES)))
  dimnames(vals) <- list(ACL_BRANCHES, ACQ_BRANCHES)
  vals[,"ACQ1"] <- fabc_c(x[1],x[4],x[7])
  vals[,"ACQ2"] <- fabc_c(x[2],x[5],x[8])
  vals[,"ACQ3"] <- fabc_c(x[3],x[6],x[9])
  vals[,"ACQ4"] <- c(0,0,0,0,1)
  return(vals)
}

createArrayAcl_c <- cmpfun(createArrayAcl)
stopifnot(all(apply(createArrayAcl_c(rep(0.5, 9)), 2, sum) == 1))

createArrayMat <- function(x)
{
  vals <- array(0, dim=c(length(MAT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES)))
  dimnames(vals) <- list(MAT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES)
  vals["NA",,] <- 1
  vals[,"ACQ1","ACL1"] <- fab_c(x[1], x[4])
  vals[,"ACQ1","ACL2"] <- fab_c(x[2], x[5])
  vals[,"ACQ1","ACL3"] <- fab_c(x[3], x[6])
  return(vals)
}

createArrayMat_c <- cmpfun(createArrayMat)
stopifnot(all(apply(createArrayMat_c(rep(0.5, 6)), c(2,3), sum) == 1))

createArrayAmt <- function(x)
{
  vals <- array(0, dim=c(length(AMT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES), length(MAT_BRANCHES)))
  dimnames(vals) <- list(AMT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES, MAT_BRANCHES)
  vals["NA",,,] <- 1
  vals[,"ACQ1","ACL1","MAT1"] <- fabc_c(x[1],x[10],x[19])
  vals[,"ACQ1","ACL2","MAT1"] <- fabc_c(x[2],x[11],x[20])
  vals[,"ACQ1","ACL3","MAT1"] <- fabc_c(x[3],x[12],x[21])
  vals[,"ACQ1","ACL1","MAT2"] <- fabc_c(x[4],x[13],x[22])
  vals[,"ACQ1","ACL2","MAT2"] <- fabc_c(x[5],x[14],x[23])
  vals[,"ACQ1","ACL3","MAT2"] <- fabc_c(x[6],x[15],x[24])
  vals[,"ACQ1","ACL1","MAT3"] <- fabc_c(x[7],x[16],x[25])
  vals[,"ACQ1","ACL2","MAT3"] <- fabc_c(x[8],x[17],x[26])
  vals[,"ACQ1","ACL3","MAT3"] <- fabc_c(x[9],x[18],x[27])
  return(vals)
}

createArrayAmt_c <- cmpfun(createArrayAmt)
stopifnot(all(apply(createArrayAmt_c(rep(0.5, 27)), c(2,3,4), sum) == 1))

createArrayInt <- function(x)
{
  vals <- array(0, dim=c(length(INT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES)))
  dimnames(vals) <- list(INT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES)
  vals["NA",,] <- 1
  vals[,"ACQ1","ACL1"] <- fa_c(x[1])
  vals[,"ACQ2","ACL1"] <- fa_c(x[2])
  vals[,"ACQ3","ACL1"] <- fa_c(x[3])
  vals[,"ACQ1","ACL2"] <- fa_c(x[4])
  vals[,"ACQ2","ACL2"] <- fa_c(x[5])
  vals[,"ACQ3","ACL2"] <- fa_c(x[6])
  vals[,"ACQ1","ACL3"] <- fa_c(x[7])
  vals[,"ACQ2","ACL3"] <- fa_c(x[8])
  vals[,"ACQ3","ACL3"] <- fa_c(x[9])
  return(vals)
}

createArrayInt_c <- cmpfun(createArrayInt)
stopifnot(all(apply(createArrayInt_c(rep(0.5, 9)), c(2,3), sum) == 1))

createArraySf <- function(x)
{
  vals <- array(0, dim=c(length(SF_BRANCHES), length(ACL_BRANCHES)))
  dimnames(vals) <- list(SF_BRANCHES, ACL_BRANCHES)
  vals[,"ACL0"] <- c(0,0,1)
  vals[,"ACL1"] <- fa_c(x[1])
  vals[,"ACL2"] <- fa_c(x[2])
  vals[,"ACL3"] <- fa_c(x[3])
  vals[,"NA"] <- c(0,0,1)
  return(vals)
}

createArraySf_c <- cmpfun(createArraySf)
stopifnot(all(apply(createArraySf_c(rep(0.5, 3)), c(2), sum) == 1))

# function to compute the residuals from the elicitee answers and the model
# x is the parameter list
# dat is the data from the elicitees
fresid <- function(x, dat)
{
  #x <- c(rep(0.5, NINDEP), rep(3, nPeople))
  resultmat <- dat

  # turn paraemters into probabilities at each node
  xacq <- fabc_nozero_c(x[1], x[2], x[3])
  acqTable <- cptable(~ACQ, values=xacq, levels=ACQ_BRANCHES)
  
  vals <- createArrayAcl_c(x[4:12])
  aclTable <- cptable(~ACL|ACQ, values=vals, levels=ACL_BRANCHES)
  
  vals <- createArrayMat_c(x[13:18])
  matTable <- cptable(~MAT|ACQ:ACL, values=vals, levels=MAT_BRANCHES)
  
  vals <- createArrayAmt_c(x[19:45])
  amtTable <- cptable(~AMT|ACQ:ACL:MAT, values=vals, levels=AMT_BRANCHES)
  
  vals <- createArrayInt_c(x[46:54])
  intTable <- cptable(~INT|ACQ:ACL, values=vals, levels=INT_BRANCHES)
  
  vals <- createArraySf_c(x[55:57])
  sfTable <- cptable(~SF|ACL, values=vals, levels=SF_BRANCHES)
  
  # create the network of nodes
  plist <- compileCPT(list(acqTable, aclTable, matTable, amtTable, intTable, sfTable))
  net1 <- grain(plist)
  
  # query the network for results
  ACQACLMAT <- querygrain(net1, nodes=c("ACQ","ACL", "MAT"), type="joint")
  ACQACLAMT <- querygrain(net1, nodes=c("ACQ","ACL", "AMT"), type="joint")
  ACQACLINT <- querygrain(net1, nodes=c("ACQ","ACL", "INT"), type="joint")
  ACQACLSF  <- querygrain(net1, nodes=c("ACQ","ACL", "SF"), type="joint")
  ACQMATAMT <- querygrain(net1, nodes=c("ACQ","MAT", "AMT"), type="joint")
  ACQMATINT <- querygrain(net1, nodes=c("ACQ","MAT", "INT"), type="joint")
  ACQMATSF  <- querygrain(net1, nodes=c("ACQ","MAT", "SF"), type="joint")
  ACQAMTINT <- querygrain(net1, nodes=c("ACQ","AMT", "INT"), type="joint")
  ACQAMTSF  <- querygrain(net1, nodes=c("ACQ","AMT", "SF"), type="joint")
  ACQINTSF  <- querygrain(net1, nodes=c("ACQ","INT", "SF"), type="joint")
  
  ACLMATAMT <- querygrain(net1, nodes=c("ACL","MAT", "AMT"), type="joint")
  ACLMATINT <- querygrain(net1, nodes=c("ACL","MAT", "INT"), type="joint")
  ACLMATSF  <- querygrain(net1, nodes=c("ACL","MAT", "SF"), type="joint")
  ACLAMTINT <- querygrain(net1, nodes=c("ACL","AMT", "INT"), type="joint")
  ACLAMTSF  <- querygrain(net1, nodes=c("ACL","AMT", "SF"), type="joint")
  ACLINTSF  <- querygrain(net1, nodes=c("ACL","INT", "SF"), type="joint")
  
  MATAMTINT <- querygrain(net1, nodes=c("MAT","AMT", "INT"), type="joint")
  MATAMTSF  <- querygrain(net1, nodes=c("MAT","AMT", "SF"), type="joint")
  MATINTSF  <- querygrain(net1, nodes=c("MAT","INT", "SF"), type="joint")
  
  AMTINTSF  <- querygrain(net1, nodes=c("AMT","INT", "SF"), type="joint")
  
  # evidence from experts
  for (i in 1:nPeople)
  {
    b <- x[NINDEP+i]
    temp <- sapply(probStatements[-indBlank][ind], function(x) eval(parse(text=x)))
    resultmat[,i] <- (dat[,i] - temp)
  }
  return(c(resultmat))
}
fresidc <- cmpfun(fresid)

# given a list of parameters, make probabilities for display
backTransform <- function(x)
{
  res <- list()

  xacq <- fabc_nozero_c(x[1], x[2], x[3])
  res$acq <- xacq
  
  vals <- createArrayAcl_c(x[4:12])
  res$acl <- vals
  
  vals <- createArrayMat_c(x[13:18])
  res$mat <- vals
  
  vals <- createArrayAmt_c(x[19:45])
  res$amt <- vals
  
  vals <- createArrayInt_c(x[46:54])
  res$int <- vals
  
  vals <- createArraySf_c(x[55:57])
  res$sf <- vals
  
  return(res)
}

################################################################################

# create scenario triplets with constraits from all possible triplets

nodeStates <- list(ACQ_BRANCHES, ACL_BRANCHES, MAT_BRANCHES,
                   AMT_BRANCHES, INT_BRANCHES, SF_BRANCHES)

triScenarios <- list()
count <- 1
for (i in 1:(NODES-2))
{
  for (j in 1:length(nodeStates[[i]]))
  {
    for (k in (i+1):(NODES-1))
    {
      for (m in 1:length(nodeStates[[k]]))
      {
        for (n in (k+1):NODES)
        {
          for (o in 1:length(nodeStates[[n]]))
          {
            temp <- c(nodeStates[[i]][j], nodeStates[[k]][m], nodeStates[[n]][o])
            # if the first level is NA in a triplet, it doesn't make sense
            if (temp[1] == "NA") next
            # if the second two are NA, then it has to be ACQ4
            if (temp[2] == "NA" && temp[3] == "NA" && temp[1] != "ACQ4") next
            # if the third is NA, then the first two must be "ACQ_" and "ACL0"
            if (temp[3] == "NA" && !(temp[2] == "ACL0" && temp[1] %in% ACQ_BRANCHES[1:3])) next
            # if the first two are ACQ_ and ACL0, the third must be NA
            if (temp[2] == "ACL0" && temp[1] %in% ACQ_BRANCHES[1:3] && temp[3] != "NA") next
            # if the second is NA, but the first and third are not, that is bad
            if (temp[2] == "NA" && temp[1] != "NA" && temp[3] != "NA") next
            if (temp[1] %in% ACQ_BRANCHES[2:4] && temp[2] != "NA") next
            if (temp[1] == "ACL0" && temp[2] != "NA") next
            triScenarios[[count]] <- c(nodeStates[[i]][j], nodeStates[[k]][m], nodeStates[[n]][o],
                                       LEVELS[i], LEVELS[k], LEVELS[n])
            count <- count + 1
          }
        }
      }
    }
  }
}

#ind <- which(sapply(triScenarios, function(x) "NA" %in% x))
#triScenarios[ind]

# create pairs of scenario triplets

dualScenarioPairs <- combn(triScenarios, 2)

# create statements of the form
# A1B1C1_A2B3D2 <- log(ABC["A1","B1","C1"] / ABD["A2","B3","D2"]) / log(b)

probStatements <- character(ncol(dualScenarioPairs))
for (j in 1:ncol(dualScenarioPairs))
{
  one <- dualScenarioPairs[1,j][[1]]
  two <- dualScenarioPairs[2,j][[1]]
  if (paste(one[1:3], collapse="") == paste(two[1:3], collapse="")) next
  probStatements[j] <- paste(paste(one[1:3], collapse=""), "_", 
                             paste(two[1:3], collapse=""),
                             " <- log(", 
                             paste(one[4:6], collapse=""), "[\"", 
                             one[1], "\",\"", one[2], "\",\"", one[3], "\"]/",
                             paste(two[4:6], collapse=""), "[\"",
                             two[1], "\",\"", two[2], "\",\"", two[3], "\"])/log(b)",
                             sep="")
}

# pick a subset of the statements to use equal to the number of questions you ask elicitees
indBlank <- which(probStatements == "")
ind <- sample(1:length(probStatements[-indBlank]), size=NQUESTIONS, replace=FALSE)

# simulate data from experts
set.seed(19191)
dat <- matrix(sample(-2:2, size=length(ind)*nPeople, replace=TRUE), nrow=length(ind), ncol=nPeople)

# create a randomLHS to analyze for a good starting value for optimization
set.seed(1976)
Y <- randomLHS(NDESIGNPOINTS, NINDEP+nPeople)
# create the log base variables on 2 to 10 instead of 0 to 1
Y[,(NINDEP+1):(NINDEP+nPeople)] <- 2 + Y[,(NINDEP+1):(NINDEP+nPeople)]*(MAXLOGBASE-MINLOGBASE)

# make a paralle cluster
cl <- makeCluster(NCORES)
# put gRain on all the cores
dummy <- clusterEvalQ(cl, {require(gRain)})
# put the data on all the cores
clusterExport(cl, c("nPeople", "probStatements","ind","indBlank",
                    "fa_c","fab_c","fabc_c","fabc_nozero_c",
                    "createArrayAcl_c",
                    "createArrayAmt_c","createArrayMat_c",
                    "createArrayInt_c","createArraySf_c",
                    "ACQ_BRANCHES","ACL_BRANCHES",
                    "MAT_BRANCHES","AMT_BRANCHES",
                    "INT_BRANCHES","SF_BRANCHES",
                    "NINDEP"))
# compute the residuals at each design point
system.time({
  Yres <- parRapply(cl, Y, fresidc, dat=dat)
})
# 758.79 seconds
stopCluster(cl)

# save the data for a stopping spot
#save.image(file.path("C:","Users","Rob","Desktop","Conjoint","Conjoint_IND.Rdata"))
#load(file.path("C:","Users","Rob","Desktop","Conjoint", "Conjoint_IND.Rdata"))

# predict the minimum sum of the squared residuals based on a GLM with quadratic terms and no interactions
Yres <- matrix(Yres, nrow=NDESIGNPOINTS, ncol=nPeople*NQUESTIONS, byrow=TRUE)
Yres2 <- apply(Yres, 1, function(x) x %*% x)
YY2 <- cbind(Y, Y^2)
g <- glm(Yres2 ~ YY2)
# Y = a + bx + cx^2
# Y has zero derivative at x=-b/(2c)
xlow <- -1*coef(g)[2:(NINDEP+nPeople+1)]/ (2*coef(g)[(NINDEP+nPeople+2):(2*(NINDEP+nPeople)+1)])
# account for problems in the minimum prediction
#  we are just looking for starting values so, not too much worry here
xlow[1:NINDEP] <-  ifelse(xlow[1:NINDEP]  < 1E-6,     1E-6,     xlow[1:NINDEP])
xlow[1:NINDEP] <-  ifelse(xlow[1:NINDEP]  > 0.999999, 0.999999, xlow[1:NINDEP])
xlow[(NINDEP+1):(NINDEP+nPeople)] <- ifelse(xlow[(NINDEP+1):(NINDEP+nPeople)] < 2,  2,  xlow[(NINDEP+1):(NINDEP+nPeople)])
xlow[(NINDEP+1):(NINDEP+nPeople)] <- ifelse(xlow[(NINDEP+1):(NINDEP+nPeople)] > 10, 2,  xlow[(NINDEP+1):(NINDEP+nPeople)])

# get the residuals at the predicted minimum
temp <- fresidc(xlow, dat=dat)

# ensure that the minimum predicted by the GLM is less than the best value from the LHS design
#  otherwise just use the best value from the LHS design
if (min(Yres2) > temp%*%temp)
{
  startVal <- xlow
} else
{
  startVal <- Y[which.min(Yres2),]
}

system.time({
  nlsres <- nls.lm(par=startVal, 
                 lower=c(rep(1E-12, NINDEP), rep(2,nPeople)),
                 upper=c(rep(0.9999999, NINDEP), rep(10, nPeople)),
                 fresidc, dat=dat,
                 control=list(maxiter=100))
})

nlsres$par
backTransform(nlsres$par)


