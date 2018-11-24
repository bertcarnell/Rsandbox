## Nerd's data analysis task

# Ito et al wild type
# Ito et al null
# david et al wild type
# three dose levels
# adenoma
# carcinoma 1
# carcinoma 2

X <- array(NA, dim=c(3,3,3))
N <- array(NA, dim=c(3,3))
X[1,1,] <- c(0,0,0)
N[1,1] <- 24
X[1,2,] <- c(2,0,0)
N[1,2] <- 23
X[1,3,] <- c(2,0,0)
N[1,3] <- 20
X[2,1,] <- c(0,1,0)
N[2,1] <- 25
X[2,2,] <- c(1,0,0)
N[2,2] <- 25
X[2,3,] <- c(6,1,1)
N[2,3] <- 31
X[3,1,] <- c(4,4,0)
N[3,1] <- 70
X[3,2,] <- c(10,5,0)
N[3,2] <- 60
X[3,3,] <- c(13,9,0)
N[3,3] <- 65

dimnames(X) <- list(c("ItoWild","ItoNull","DavidWild"),
  c("0","100","500"), c("adenoma", "carncinoma1","carcinoma2"))
dimnames(N) <- list(c("ItoWild","ItoNull","DavidWild"),
  c("0","100","500"))

# fisher's exact test versus control
#first Ito null vs control
fisher.test( cbind(X[2,1:2,1], N[2,1:2]-X[2,1:2,1]) )
fisher.test( cbind(X[2,c(1,3),1], N[2,c(1,3)]-X[2,c(1,3),1]) )
# middle Ito wild vs control
fisher.test( cbind(X[1,1:2,1], N[1,1:2]-X[1,1:2,1]) )
fisher.test( cbind(X[1,c(1,3),1], N[1,c(1,3)]-X[1,c(1,3),1]) )
# david last
fisher.test( cbind(X[3,1:2,1], N[3,1:2]-X[3,1:2,1]) )
fisher.test( cbind(X[3,c(1,3),1], N[3,c(1,3)]-X[3,c(1,3),1]) )

# are the controls different in Ito null to David wild?
fisher.test( cbind(X[2:3,1,1], N[2:3,1]-X[2:3,1,1]) )
# are the controls different in Ito wild to David wild?
fisher.test( cbind(X[c(1,3),1,1], N[c(1,3),1]-X[c(1,3),1,1]) )
# are the 500 ppm levels different in Ito null to David wild?
fisher.test( cbind(X[2:3,3,1], N[2:3,3]-X[2:3,3,1]) )
# are the 500 ppm levels different in Ito wild to David wild?
fisher.test( cbind(X[c(1,3),3,1], N[c(1,3),3]-X[c(1,3),3,1]) )

# are any of the contols different?
fisher.test( cbind(X[,1,1], N[,1]-X[,1,1]) )
# are any of the 500 ppm levels different?
fisher.test( cbind(X[,3,1], N[,3]-X[,3,1]) )

Y <- data.frame(incidence=c(X[,,1]),
  total=c(N),
  study=as.factor(rep(c("ItoWild","ItoNull","DavidWild"), times=3)),
  dose=rep(c(0,100,500), each=3))
require(survival)
g <- glm(cbind(Y$incidence, Y$total-Y$incidence) ~ Y$dose + Y$study,
  family=binomial(link="logit"))

g <- glm(cbind(Y$incidence, Y$total-Y$incidence) ~ Y$dose*Y$study,
  family=binomial(link="logit"))

# look up cochrane armitage test for dose versus control

a reexamination of the ppar-alpha activation mode of action as a basis for assessing
human cancer risks of environmental contaminants
kathryn guyton
environmental health perspectives (vol 117, number 11, Nov 2009

# multivariate response instead of additive response


