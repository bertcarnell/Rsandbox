rm(list=ls())

#source("http://bioconductor.org/biocLite.R"); biocLite(c("graph","RBGL","Rgraphviz"))
require(XML)
require(gRain)
require(compiler)
require(lhs)
require(minpack.lm)
require(parallel)

################################################################################

LEVELS <- c("ACQ","ACL","MAT","AMT","INT","SF")
NODES <- length(LEVELS)
ACQ_BRANCHES <- paste("ACQ", 1:4, sep="")
ACL_BRANCHES <- c(paste("ACL", 0:3, sep=""), "NA")
MAT_BRANCHES <- c(paste("MAT", 1:3, sep=""), "NA")
AMT_BRANCHES <- c(paste("AMT", 1:4, sep=""), "NA")
INT_BRANCHES <- c("Y","N","NA")
SF_BRANCHES <- c("Y","N","NA")

################################################################################
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

createArrayAcl <- function(x)
{
  vals <- array(0, dim=c(length(ACL_BRANCHES), length(ACQ_BRANCHES)))
  dimnames(vals) <- list(ACL_BRANCHES, ACQ_BRANCHES)
  vals[,"ACQ1"] <- fabc(x[1],x[4],x[7])
  vals[,"ACQ2"] <- fabc(x[2],x[5],x[8])
  vals[,"ACQ3"] <- fabc(x[3],x[6],x[9])
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
  vals[,"ACQ1","ACL1"] <- fab(x[1], x[4])
  vals[,"ACQ1","ACL2"] <- fab(x[2], x[5])
  vals[,"ACQ1","ACL3"] <- fab(x[3], x[6])
  return(vals)
}

createArrayMat_c <- cmpfun(createArrayMat)
stopifnot(all(apply(createArrayMat_c(rep(0.5, 6)), c(2,3), sum) == 1))

createArrayAmt <- function(x)
{
  vals <- array(0, dim=c(length(AMT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES), length(MAT_BRANCHES)))
  dimnames(vals) <- list(AMT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES, MAT_BRANCHES)
  vals["NA",,,] <- 1
  vals[,"ACQ1","ACL1","MAT1"] <- fabc(x[1],x[10],x[19])
  vals[,"ACQ1","ACL2","MAT1"] <- fabc(x[2],x[11],x[20])
  vals[,"ACQ1","ACL3","MAT1"] <- fabc(x[3],x[12],x[21])
  vals[,"ACQ1","ACL1","MAT2"] <- fabc(x[4],x[13],x[22])
  vals[,"ACQ1","ACL2","MAT2"] <- fabc(x[5],x[14],x[23])
  vals[,"ACQ1","ACL3","MAT2"] <- fabc(x[6],x[15],x[24])
  vals[,"ACQ1","ACL1","MAT3"] <- fabc(x[7],x[16],x[25])
  vals[,"ACQ1","ACL2","MAT3"] <- fabc(x[8],x[17],x[26])
  vals[,"ACQ1","ACL3","MAT3"] <- fabc(x[9],x[18],x[27])
  return(vals)
}

createArrayAmt_c <- cmpfun(createArrayAmt)
stopifnot(all(apply(createArrayAmt_c(rep(0.5, 27)), c(2,3,4), sum) == 1))

createArrayInt <- function(x)
{
  vals <- array(0, dim=c(length(INT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES)))
  dimnames(vals) <- list(INT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES)
  vals["NA",,] <- 1
  vals[,"ACQ1","ACL1"] <- fa(x[1])
  vals[,"ACQ2","ACL1"] <- fa(x[2])
  vals[,"ACQ3","ACL1"] <- fa(x[3])
  vals[,"ACQ1","ACL2"] <- fa(x[4])
  vals[,"ACQ2","ACL2"] <- fa(x[5])
  vals[,"ACQ3","ACL2"] <- fa(x[6])
  vals[,"ACQ1","ACL3"] <- fa(x[7])
  vals[,"ACQ2","ACL3"] <- fa(x[8])
  vals[,"ACQ3","ACL3"] <- fa(x[9])
  return(vals)
}

createArrayInt_c <- cmpfun(createArrayInt)
stopifnot(all(apply(createArrayInt_c(rep(0.5, 9)), c(2,3), sum) == 1))

createArraySf <- function(x)
{
  vals <- array(0, dim=c(length(SF_BRANCHES), length(ACL_BRANCHES)))
  dimnames(vals) <- list(SF_BRANCHES, ACL_BRANCHES)
  vals[,"ACL0"] <- c(0,0,1)
  vals[,"ACL1"] <- fa(x[1])
  vals[,"ACL2"] <- fa(x[2])
  vals[,"ACL3"] <- fa(x[3])
  vals[,"NA"] <- c(0,0,1)
  return(vals)
}

createArraySf_c <- cmpfun(createArraySf)
stopifnot(all(apply(createArraySf_c(rep(0.5, 3)), c(2), sum) == 1))

################################################################################

vals <- c(1/4,1/4,1/4,1/4)
acqTable <- cptable(~ACQ, values=vals, levels=ACQ_BRANCHES)
vals <- matrix(c(1/4,1/4,1/4,1/4,0,
                 1/4,1/4,1/4,1/4,0,
                 1/4,1/4,1/4,1/4,0,
                 0,  0,  0,  0,  1), nrow=length(ACQ_BRANCHES), ncol=length(ACL_BRANCHES), byrow=TRUE)
dimnames(vals) <- list(ACQ_BRANCHES, ACL_BRANCHES)
aclTable <- cptable(~ACL|ACQ, values=vals, levels=ACL_BRANCHES)

vals <- array(0, dim=c(length(MAT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES)))
dimnames(vals) <- list(MAT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES)
vals["NA",,] <- 1
vals[,"ACQ1","ACL1"] <- c(1/3,1/3,1/3,0)
vals[,"ACQ1","ACL2"] <- c(1/3,1/3,1/3,0)
vals[,"ACQ1","ACL3"] <- c(1/3,1/3,1/3,0)
matTable <- cptable(~MAT|ACQ:ACL, values=vals, levels=MAT_BRANCHES)

vals <- array(0, dim=c(length(AMT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES), length(MAT_BRANCHES)))
dimnames(vals) <- list(AMT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES, MAT_BRANCHES)
vals["NA",,,] <- 1
vals[,"ACQ1","ACL1","MAT1"] <- c(1/4,1/4,1/4,1/4,0)
vals[,"ACQ1","ACL2","MAT1"] <- c(1/4,1/4,1/4,1/4,0)
vals[,"ACQ1","ACL3","MAT1"] <- c(1/4,1/4,1/4,1/4,0)
vals[,"ACQ1","ACL1","MAT2"] <- c(1/4,1/4,1/4,1/4,0)
vals[,"ACQ1","ACL2","MAT2"] <- c(1/4,1/4,1/4,1/4,0)
vals[,"ACQ1","ACL3","MAT2"] <- c(1/4,1/4,1/4,1/4,0)
vals[,"ACQ1","ACL1","MAT3"] <- c(1/4,1/4,1/4,1/4,0)
vals[,"ACQ1","ACL2","MAT3"] <- c(1/4,1/4,1/4,1/4,0)
vals[,"ACQ1","ACL3","MAT3"] <- c(1/4,1/4,1/4,1/4,0)
amtTable <- cptable(~AMT|ACQ:ACL:MAT, values=vals, levels=MAT_BRANCHES)

vals <- array(0, dim=c(length(INT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES)))
dimnames(vals) <- list(INT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES)
vals["NA",,] <- 1
vals[,"ACQ1","ACL1"] <- c(0.5,0.5,0)
vals[,"ACQ2","ACL1"] <- c(0.5,0.5,0)
vals[,"ACQ3","ACL1"] <- c(0.5,0.5,0)
vals[,"ACQ1","ACL2"] <- c(0.5,0.5,0)
vals[,"ACQ2","ACL2"] <- c(0.5,0.5,0)
vals[,"ACQ3","ACL2"] <- c(0.5,0.5,0)
vals[,"ACQ1","ACL3"] <- c(0.5,0.5,0)
vals[,"ACQ2","ACL3"] <- c(0.5,0.5,0)
vals[,"ACQ3","ACL3"] <- c(0.5,0.5,0)
intTable <- cptable(~INT|ACQ:ACL, values=vals, levels=INT_BRANCHES)

vals <- matrix(c(0,  0,  1,
                 1/2,1/2,0,
                 1/2,1/2,0,
                 1/2,1/2,0,
                 0,  0,  1), nrow=length(ACL_BRANCHES), ncol=length(SF_BRANCHES), byrow=TRUE)
dimnames(vals) <- list(ACL_BRANCHES, SF_BRANCHES)
sfTable <- cptable(~SF|ACL, values=vals, levels=SF_BRANCHES)

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

# pick a subset of the columns to use
indBlank <- which(probStatements == "")
set.seed(19191)
ind <- sample(1:length(probStatements[-indBlank]), size=200, replace=FALSE)

nPeople <- 9
dat <- matrix(sample(-2:2, size=1, replace=TRUE), nrow=length(ind), ncol=nPeople)

fresid <- function(x, dat)
{
  #x <- c(rep(0.5, 59), rep(3,9))
  
  fa <- function(x) c(x, 1-x, 0)
  fab <- function(x,y) c(x, y*(1-x), 1-x-y*(1-x), 0)
  fabc <- function(x,y,z) c(x, y*(1-x), z*(1-x-y*(1-x)), 1-x-y*(1-x)-z*(1-x-y*(1-x)), 0)
  fabc_nozero <- function(x,y,z) c(x, y*(1-x), z*(1-x-y*(1-x)), 1-x-y*(1-x)-z*(1-x-y*(1-x)))
  
  resultmat <- dat
  
  xacq <- fabc_nozero(x[1], x[2], x[3])
  acqTable <- cptable(~ACQ, values=xacq, levels=ACQ_BRANCHES)
  
  vals <- array(0, dim=c(length(ACL_BRANCHES), length(ACQ_BRANCHES)))
  dimnames(vals) <- list(ACL_BRANCHES, ACQ_BRANCHES)
  vals[,"ACQ1"] <- fabc(x[6],x[9],x[12])
  vals[,"ACQ2"] <- fabc(x[7],x[10],x[13])
  vals[,"ACQ3"] <- fabc(x[8],x[11],x[14])
  vals[,"ACQ4"] <- c(0,0,0,0,1)
  aclTable <- cptable(~ACL|ACQ, values=vals, levels=ACL_BRANCHES)

  vals <- array(0, dim=c(length(MAT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES)))
  dimnames(vals) <- list(MAT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES)
  vals["NA",,] <- 1
  vals[,"ACQ1","ACL1"] <- fab(x[15], x[18])
  vals[,"ACQ1","ACL2"] <- fab(x[16], x[19])
  vals[,"ACQ1","ACL3"] <- fab(x[17], x[20])
  matTable <- cptable(~MAT|ACQ:ACL, values=vals, levels=MAT_BRANCHES)
  
  vals <- array(0, dim=c(length(AMT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES), length(MAT_BRANCHES)))
  dimnames(vals) <- list(AMT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES, MAT_BRANCHES)
  vals["NA",,,] <- 1
  vals[,"ACQ1","ACL1","MAT1"] <- fabc(x[21],x[30],x[39])
  vals[,"ACQ1","ACL2","MAT1"] <- fabc(x[22],x[31],x[40])
  vals[,"ACQ1","ACL3","MAT1"] <- fabc(x[23],x[32],x[41])
  vals[,"ACQ1","ACL1","MAT2"] <- fabc(x[24],x[33],x[42])
  vals[,"ACQ1","ACL2","MAT2"] <- fabc(x[25],x[34],x[43])
  vals[,"ACQ1","ACL3","MAT2"] <- fabc(x[26],x[35],x[44])
  vals[,"ACQ1","ACL1","MAT3"] <- fabc(x[27],x[36],x[45])
  vals[,"ACQ1","ACL2","MAT3"] <- fabc(x[28],x[37],x[46])
  vals[,"ACQ1","ACL3","MAT3"] <- fabc(x[29],x[38],x[47])
  amtTable <- cptable(~AMT|ACQ:ACL:MAT, values=vals, levels=AMT_BRANCHES)
  
  vals <- array(0, dim=c(length(INT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES)))
  dimnames(vals) <- list(INT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES)
  vals["NA",,] <- 1
  vals[,"ACQ1","ACL1"] <- fa(x[48])
  vals[,"ACQ2","ACL1"] <- fa(x[49])
  vals[,"ACQ3","ACL1"] <- fa(x[50])
  vals[,"ACQ1","ACL2"] <- fa(x[51])
  vals[,"ACQ2","ACL2"] <- fa(x[52])
  vals[,"ACQ3","ACL2"] <- fa(x[53])
  vals[,"ACQ1","ACL3"] <- fa(x[54])
  vals[,"ACQ2","ACL3"] <- fa(x[55])
  vals[,"ACQ3","ACL3"] <- fa(x[56])
  intTable <- cptable(~INT|ACQ:ACL, values=vals, levels=INT_BRANCHES)
  
  vals <- array(0, dim=c(length(SF_BRANCHES), length(ACL_BRANCHES)))
  dimnames(vals) <- list(SF_BRANCHES, ACL_BRANCHES)
  vals[,"ACL0"] <- c(0,0,1)
  vals[,"ACL1"] <- fa(x[57])
  vals[,"ACL2"] <- fa(x[58])
  vals[,"ACL3"] <- fa(x[59])
  vals[,"NA"] <- c(0,0,1)
  sfTable <- cptable(~SF|ACL, values=vals, levels=SF_BRANCHES)
  
  plist <- compileCPT(list(acqTable, aclTable, matTable, amtTable, intTable, sfTable))
  net1 <- grain(plist)
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
    b <- x[59+i]
    temp <- sapply(probStatements[-indBlank][ind], function(x) eval(parse(text=x)))
    resultmat[,i] <- (dat[,i] - temp)
  }
  return(c(resultmat))
}
fresidc <- cmpfun(fresid)


backTransform <- function(x)
{
  res <- list()

  fa <- function(x) c(x, 1-x, 0)
  fab <- function(x,y) c(x, y*(1-x), 1-x-y*(1-x), 0)
  fabc <- function(x,y,z) c(x, y*(1-x), z*(1-x-y*(1-x)), 1-x-y*(1-x)-z*(1-x-y*(1-x)), 0)
  fabc_nozero <- function(x,y,z) c(x, y*(1-x), z*(1-x-y*(1-x)), 1-x-y*(1-x)-z*(1-x-y*(1-x)))
  
  xacq <- fabc_nozero(x[1], x[2], x[3])
  res$acq <- xacq
  
  vals <- array(0, dim=c(length(ACL_BRANCHES), length(ACQ_BRANCHES)))
  dimnames(vals) <- list(ACL_BRANCHES, ACQ_BRANCHES)
  vals[,"ACQ1"] <- fabc(x[6],x[9],x[12])
  vals[,"ACQ2"] <- fabc(x[7],x[10],x[13])
  vals[,"ACQ3"] <- fabc(x[8],x[11],x[14])
  vals[,"ACQ4"] <- c(0,0,0,0,1)
  res$acl <- vals
  
  vals <- array(0, dim=c(length(MAT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES)))
  dimnames(vals) <- list(MAT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES)
  vals["NA",,] <- 1
  vals[,"ACQ1","ACL1"] <- fab(x[15], x[18])
  vals[,"ACQ1","ACL2"] <- fab(x[16], x[19])
  vals[,"ACQ1","ACL3"] <- fab(x[17], x[20])
  res$mat <- vals
  
  vals <- array(0, dim=c(length(AMT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES), length(MAT_BRANCHES)))
  dimnames(vals) <- list(AMT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES, MAT_BRANCHES)
  vals["NA",,,] <- 1
  vals[,"ACQ1","ACL1","MAT1"] <- fabc(x[21],x[30],x[39])
  vals[,"ACQ1","ACL2","MAT1"] <- fabc(x[22],x[31],x[40])
  vals[,"ACQ1","ACL3","MAT1"] <- fabc(x[23],x[32],x[41])
  vals[,"ACQ1","ACL1","MAT2"] <- fabc(x[24],x[33],x[42])
  vals[,"ACQ1","ACL2","MAT2"] <- fabc(x[25],x[34],x[43])
  vals[,"ACQ1","ACL3","MAT2"] <- fabc(x[26],x[35],x[44])
  vals[,"ACQ1","ACL1","MAT3"] <- fabc(x[27],x[36],x[45])
  vals[,"ACQ1","ACL2","MAT3"] <- fabc(x[28],x[37],x[46])
  vals[,"ACQ1","ACL3","MAT3"] <- fabc(x[29],x[38],x[47])
  res$amt <- vals
  
  vals <- array(0, dim=c(length(INT_BRANCHES), length(ACQ_BRANCHES), length(ACL_BRANCHES)))
  dimnames(vals) <- list(INT_BRANCHES, ACQ_BRANCHES, ACL_BRANCHES)
  vals["NA",,] <- 1
  vals[,"ACQ1","ACL1"] <- fa(x[48])
  vals[,"ACQ2","ACL1"] <- fa(x[49])
  vals[,"ACQ3","ACL1"] <- fa(x[50])
  vals[,"ACQ1","ACL2"] <- fa(x[51])
  vals[,"ACQ2","ACL2"] <- fa(x[52])
  vals[,"ACQ3","ACL2"] <- fa(x[53])
  vals[,"ACQ1","ACL3"] <- fa(x[54])
  vals[,"ACQ2","ACL3"] <- fa(x[55])
  vals[,"ACQ3","ACL3"] <- fa(x[56])
  res$int <- vals
  
  vals <- array(0, dim=c(length(SF_BRANCHES), length(ACL_BRANCHES)))
  dimnames(vals) <- list(SF_BRANCHES, ACL_BRANCHES)
  vals[,"ACL0"] <- c(0,0,1)
  vals[,"ACL1"] <- fa(x[57])
  vals[,"ACL2"] <- fa(x[58])
  vals[,"ACL3"] <- fa(x[59])
  vals[,"NA"] <- c(0,0,1)
  res$sf <- vals
  
  return(res)
}

set.seed(1976)
Y <- randomLHS(100, 68)
Y[,60:68] <- 2 + Y[,60:68]*(10-2)
Yres <- apply(Y, 1, fresidc, dat=dat)

Yres2 <- apply(Yres, 2, function(x) x %*% x)

system.time({
nlsres <- nls.lm(par=Y[which.min(Yres2),], 
                 lower=c(rep(1E-12, 59), rep(2,nPeople)),
                 upper=c(rep(0.9999999, 59), rep(10, nPeople)),
                 fresidc, dat=dat,
                 control=list(maxiter=5))
}) # 587.55 without convergence

system.time({
  nlsres <- nls.lm(par=Y[which.min(Yres2),], 
                   lower=c(rep(1E-12, 59), rep(2,nPeople)),
                   upper=c(rep(0.9999999, 59), rep(10, nPeople)),
                   fresidc, dat=dat,
                   control=list(maxiter=10))
}) # 29965.66 without convergence

nlsres$par
nlsres


# did I have any way to estimate ACQ1 vs ACQ2 vs ACQ3?

# finish functionizing the underlying of fresid and backtransform

