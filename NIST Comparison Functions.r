################################################################################
# Program Name:  NIST Comparison Functions.R
# Purpose:       To compare R values with certified values from NIST
# Author:        Rob Carnell
# Date:          August 2006
#
# Required Packages: gsubfn, polynom, R2HTML
# R version:     >= 2.3.0
#
# Modified:
# Date Modified:
#
################################################################################

require(gsubfn)
require(polynom)
require(R2HTML)

################################################################################
## Utility Functions ##
#######################

fr1 <- function(M){
  n <- length(M)
  return(sum((M[2:n]-mean(M))*(M[1:(n-1)]-mean(M)))/var(M)/(n-1))
}
  
getLMCoeffsFromOrthoCoeffs <- function(lm.ortho, xdata){
  # check inputs
  if(attr(lm.ortho, "class")!="lm")
    stop("lm.ortho must be of the linear model class")
  # function must have been defined as y~poly(x, degree=d)
  p <- lm.ortho$model$pol
  d <- max(attr(p, "degree"))
  if(length(xdata)<=d) stop("there must be more values in xdata than the degree")
  # create the orthogonal polynomials
  polx <- poly.orth(xdata, degree=d)
  # modify the first value
  polx <- c(1, polx[2:(d+1)])
  # add the orthogonal coefficients times the polynomials together
  lm.coeffs <- 0
  for(i in 1:(d+1)){
    lm.coeffs <- lm.coeffs + lm.ortho$coeff[i]*polx[[i]]
  }
  # create the linear (non-orthogonal) coefficient stderrs from the
  # orthogonal coefficient std errs
  lm.ortho.stderr <- summary(lm.ortho)$coeff[,2]
  lm.stderr <- numeric(d+1)
  for(i in 1:(d+1)){
    for(j in i:(d+1)){
      # sum the variances of the orthogonal coefficients times their
      # polynomial term coefficient squared
      # and take the square root
      lm.stderr[i] <- lm.stderr[i] + (lm.ortho.stderr[j])^2*(c(polx[[j]])^2)[i]
    }
  }
  # return the coefficients for the linear (non-orthogonal) model
  result <- data.frame(cbind(c(lm.coeffs), sqrt(c(lm.stderr))))
  names(result) <- c("est", "stderr")
  return(result)
}

LRE.mod <- function(test, known, use.LAE=TRUE){
  # based on the function LRE in the accuracy package
  # Micah Altman Micah_Altman@harvard.edu <URL:
  # http://www.hmdc.harvard.edu/micah_altman/>, Michael McDonald
  n <- length(test)
  if(length(known)!=1 & length(known)!=n)
    stop("arguments are not consistent lengths")
  ind <- which(test>0 & known>0)
  ind2 <- which(test<0 & known<0)
  ind3 <- which((test>0 & known<0) |
                (test<0 & known>0))
  ind4 <- which(known==0 & test!=0)
  ind5 <- which(known!=0 & test==0)
  indna <- which(is.na(test) | is.na(known))
  result <- numeric(n)
  result[ind] <- -1 * log10(abs(test[ind] - known[ind])/known[ind])
  result[ind2] <- -1 * log10(abs((-1 * test[ind2]) - (-1 * known[ind2]))/(-1 * known[ind2]))
  result[ind3] <- -Inf # complete disagreement
  if(use.LAE){
    result[ind5] <- -1 * log10(abs(test[ind5] - known[ind5]))
    result[ind4] <- -1 * log10(abs(test[ind4]))
  } else {
    result[ind5] <- NA
    result[ind4] <- NA
  }
  result[indna] <- NA
  return(result)
}

################################################################################
## Univariate ##
################

compareUnivariate <- function(dataSet=NULL, outputHTML=FALSE, defaultPath=NULL,
                              fileName=NULL, htmlTitle="default",
                              compareTitle="default", reportDigits=15){

  if(outputHTML & (is.null(defaultPath) | is.null(fileName)))
    stop("defaultPath and fileName must be defined if HTML ouput is desired")
  if(htmlTitle=="default") htmlTitle <- "Univariate NIST Comparison"
  if(compareTitle=="default") compareTitle <- "Univariate NIST Comparison"

  if(outputHTML)
  {
    outFile <- HTMLInitFile(outdir=defaultPath, filename=fileName,
               extension="html", HTMLframe=FALSE, BackGroundColor = "FFFFFF",
               BackGroundImg = "", Title = htmlTitle,
               CSSFile="R2HTML.css", useLaTeX=TRUE, useGrid=TRUE)
    HTML(paste("<H1>", compareTitle, "</H1>"))
    HTML("<B>Notes:</B>")
    HTML("<MENU>")
    HTML("<LI>Differences are calculated as (NIST Standard - R Result):")
    HTML("<LI>LRE (Log Relative Error) gives the approximate number of significant digits to which the values agree.  Inf indicates complete agreement.  Negative numbers indicate disagreement.")
    HTML("<LI>Values are rounded to 15 significant digits as in the NIST data set")
    HTML("<LI>The r1 statistic is the lag 1 autocorrelation coefficient")
    HTML("</MENU>")
  }
  
  # path to data set
  std.data.base <- "http://www.itl.nist.gov/div898/strd"
  univ.data.base <- paste(std.data.base, "/univ/data", sep="")
  univ.data.pages <- c("PiDigits.dat", "Lottery.dat", "Lew.dat",
                         "Mavro.dat", "Michelso.dat", "NumAcc1.dat",
                         "NumAcc2.dat", "NumAcc3.dat", "NumAcc4.dat")
  if(!is.null(dataSet))
  {
    ind <- which(univ.data.pages %in% dataSet)
    if(length(ind)==0 | length(ind)!=length(dataSet))
      stop("Selected data set(s) is(are) not one of the options")
    univ.data.pages <- univ.data.pages[ind]
  }
  univ.data <- paste(univ.data.base, univ.data.pages, sep="/")

  ##############################################################################

  for(i in 1:length(univ.data))
  {
    # Read data
    filePath <- url(univ.data[i])
    Afile <- readLines(con=filePath, n=100, ok=TRUE)
    ind.cert.val <- grep("Certified Values: lines", Afile)
    ind.data <- grep("Data[[:blank:]]*: lines", Afile)
    ind.cert.val <- as.numeric(unlist(strapply(Afile[ind.cert.val], "[0-9]+")))
    ind.data <- as.numeric(unlist(strapply(Afile[ind.data], "[0-9]+")))

    A <- read.table(filePath, skip=(ind.data[1]-1), strip.white=TRUE)
    A <- as.numeric(A[,1])
    for(j in 0:2){
      Afile[ind.cert.val[1]+j] <-
        gsub("\\(exact\\)", "", Afile[ind.cert.val[1]+j])
    }
    ybar.NIST <- as.numeric(unlist(strsplit(Afile[ind.cert.val[1]], ":"))[2])
    s.NIST <- as.numeric(unlist(strsplit(Afile[ind.cert.val[1]+1], ":"))[2])
    r1.NIST <- as.numeric(unlist(strsplit(Afile[ind.cert.val[1]+2], ":"))[2])

    result <- data.frame(matrix(c(ybar.NIST, mean(A), 0, 0,
                                  s.NIST, sd(A), 0, 0,
                                  r1.NIST, fr1(A), 0, 0),
                                  nrow=3, ncol=4, byrow=TRUE))
    result[,3] <- result[,1]-result[,2]
    result[,4] <- LRE.mod(result[,2], result[,1])
    names(result) <- c("NIST Value", "R Value", "Difference", "LRE")
    rownames(result) <- c("Mean", "Std Dev", "r1")
    result <- signif(result, digits=15)

    if(outputHTML)
    {
      HTML(paste("Dataset:<B>", univ.data.pages[i], "</B>"))
      HTML(result, Border = 1, innerBorder=1, classfirstline = "firstline",
           classfirstcolumn = "firstcolumn", classcellinside = "cellinside",
           append = TRUE, align = "center", caption = "",
           captionalign = "bottom", classcaption = "captiondataframe",
           classtable = "dataframe", digits=reportDigits, nsmall = 0, big.mark = "",
           big.interval = 3, decimal.mark = ".", sortableDF=FALSE)
    } else if(length(dataSet)==1){
      return(result)
    } else {
      print("------------------------------")
      print(univ.data.pages[i])
      print(result, digits=reportDigits)
    }
  }

  if(outputHTML)
  {
    HTMLEndFile()
    browseURL(paste(defaultPath, "//", fileName, ".html", sep=""),
             browser=getOption("browser"))
  }
}


################################################################################
## Linear Regression ##
#######################

compareLinRegress <- function(dataSet=NULL, outputHTML=FALSE, defaultPath=NULL,
                              fileName=NULL, htmlTitle="default",
                              compareTitle="default", reportDigits=15){

  if(outputHTML & (is.null(defaultPath) | is.null(fileName)))
    stop("defaultPath and fileName must be defined if HTML ouput is desired")
  if(htmlTitle=="default") htmlTitle <- "Linear Regression NIST Comparison"
  if(compareTitle=="default") compareTitle <- "Linear Regression NIST Comparison"

  if(outputHTML)
  {
    outFile <- HTMLInitFile(outdir=defaultPath, filename=fileName,
               extension="html", HTMLframe=FALSE, BackGroundColor = "FFFFFF",
               BackGroundImg = "", Title = htmlTitle,
               CSSFile="R2HTML.css", useLaTeX=TRUE, useGrid=TRUE)
    HTML(paste("<H1>", compareTitle, "</H1>"))
    HTML("<B>Notes:</B>")
    HTML("<MENU>")
    HTML("<LI>Differences are calculated as (NIST Standard - R Result):")
    HTML("<LI>LRE (Log Relative Error) gives the approximate number of significant digits to which the values agree.  Inf indicates complete agreement.  Negative numbers indicate disagreement.")
    HTML("<LI>Values are rounded to 15 significant digits as in the NIST data set")
    HTML("<L1>Orthogonal Regression is used for the Pontius.dat, Filip.dat, and Wampler datasets")
    HTML("</MENU>")
  }

  # path to data set
  std.data.base <- "http://www.itl.nist.gov/div898/strd"
  reg.data.base <- paste(std.data.base, "/lls/data/LINKS/DATA", sep="")
  reg.data.pages <- c("Norris.dat", "Pontius.dat", "NoInt1.dat", "NoInt2.dat",
                      "Filip.dat", "Longley.dat", "Wampler1.dat",
                      "Wampler2.dat", "Wampler3.dat", "Wampler4.dat",
                      "Wampler5.dat")

  param <-      c(2, 3, 2, 2, 11, 7, rep(6, 5))
  param.NIST <- c(2, 3, 1, 1, 11, 7, rep(6, 5))
  posit.NIST <- c(12, 12, 11, 12, 12, 12, rep(12, 5))
  model <-      c("V1~V2", NA, "V1~V2-1", "V1~V2-1", NA,
                  "V1~V2+V3+V4+V5+V6+V7", rep(NA, 5))

  if(!is.null(dataSet))
  {
    ind <- which(reg.data.pages %in% dataSet)
    if(length(ind)==0 | length(ind)!=length(dataSet))
      stop("Selected data set(s) is(are) not one of the options")
    param <- param[ind]
    param.NIST <- param.NIST[ind]
    posit.NIST <- posit.NIST[ind]
    model <- model[ind]
    reg.data.pages <- reg.data.pages[ind]
  }
  reg.data <- paste(reg.data.base, reg.data.pages, sep="/")

  ##############################################################################

  for(i in 1:length(reg.data))
  {
    # Read data
    filePath <- url(reg.data[i])
    Afile <- readLines(con=filePath, n=100, ok=TRUE)
    ind.cert.val <- grep("Certified Values[[:blank:]]*[[:punct:]]lines", Afile)
    ind.data <- grep("Data[[:blank:]]*[[:punct:]]lines", Afile)
    ind.cert.val <- as.numeric(unlist(strapply(Afile[ind.cert.val], "[0-9]+")))
    ind.data <- as.numeric(unlist(strapply(Afile[ind.data], "[0-9]+")))

    B.NIST <- matrix(0, nrow=param.NIST[i], ncol=2)
    for(j in 1:param.NIST[i]){
      B.NIST[j,1] <- as.numeric(unlist(strsplit(Afile[ind.cert.val[1]+j-1],
                                              "[[:blank:]]+"))[3])
      B.NIST[j,2] <- as.numeric(unlist(strsplit(Afile[ind.cert.val[1]+j-1],
                                              "[[:blank:]]+"))[4])
    }
    e.sd.NIST <-
      as.numeric(unlist(strsplit(Afile[ind.cert.val[1]+param.NIST[i]+2],
                                 "[[:blank:]]+"))[4])
    r2.NIST <-
      as.numeric(unlist(strsplit(Afile[ind.cert.val[1]+param.NIST[i]+4],
                                 "[[:blank:]]+"))[3])
    if(reg.data.pages[i] %in% c("Wampler1.dat", "Wampler2.dat")){
      reg.NIST <- as.numeric(unlist(strsplit(Afile[ind.cert.val[1] +
                                             param.NIST[i] + posit.NIST[i]],
                                             "[[:blank:]]+"))[2:4])
      reg.NIST[4] <- NA
    } else {
      reg.NIST <- as.numeric(unlist(strsplit(Afile[ind.cert.val[1] +
                                             param.NIST[i] + posit.NIST[i]],
                                             "[[:blank:]]+"))[2:5])
    }
    res.NIST <- as.numeric(unlist(strsplit(Afile[ind.cert.val[1] +
                                           param.NIST[i] + posit.NIST[i]+1],
                                           "[[:blank:]]+"))[2:4])

    A <- read.table(filePath, skip=(ind.data[1]-1), strip.white=TRUE)
    if(!is.na(model[i])){
      lm.data <- lm(formula(model[i]), A)
    } else lm.data <- lm(V1 ~ poly(V2, degree=(param[i]-1)), A)

    sum.lm.data <- summary(lm.data)
    anov.lm.data <- anova(lm.data)
    if(!is.na(model[i])){
      lm.table <- sum.lm.data$coeff
    } else lm.table <- getLMCoeffsFromOrthoCoeffs(lm.data, A$V2)
    sig <- sum.lm.data$sigma
    r2 <- sum.lm.data$r.squared
    if(!is.na(model[i])){
      reg <- c(sum(anov.lm.data$Df[1:(param[i]-1)]),
               sum(anov.lm.data$Sum[1:(param[i]-1)]), NA,
               sum.lm.data$fstatistic[1])
      reg[3] <- reg[2]/reg[1]
      res <- c(anov.lm.data$Df[param[i]], anov.lm.data$Sum[param[i]],
               anov.lm.data$Mean[param[i]])
    } else {
      reg <- c(anov.lm.data$Df[1], anov.lm.data$Sum[1], anov.lm.data$Mean[1],
               anov.lm.data$F[1])
      res <- c(anov.lm.data$Df[2], anov.lm.data$Sum[2], anov.lm.data$Mean[2])
    }

    result <- data.frame(matrix(nrow=(2*param.NIST[i] + 2 + length(reg.NIST) +
                                      length(res.NIST)), ncol=4))
    result[,1] <- c(B.NIST[,1], B.NIST[,2], e.sd.NIST, r2.NIST,
                    reg.NIST, res.NIST)
    result[,2] <- c(lm.table[,1], lm.table[,2], sig, r2, reg, res)
    result[,3] <- result[,1] - result[,2]
    result[,4] <- LRE.mod(result[,2], result[,1])
    names(result) <- c("NIST Value", "R Value", "Difference", "LRE")
    rownames(result) <- c(paste("Estimate", 1:length(B.NIST[,1]), sep=""),
                          paste("Std Dev", 1:length(B.NIST[,2]), sep=""),
                          "Residual Std Dev", "R^2", "Reg df", "SSR", "MSR", "F",
                          "Res df", "SSE", "MSE")
    result <- signif(result, digits=15)

    if(outputHTML)
    {
      HTML(paste("Dataset:<B>", reg.data.pages[i], "</B>"))
      HTML(result, Border = 1, innerBorder=1, classfirstline = "firstline",
           classfirstcolumn = "firstcolumn", classcellinside = "cellinside",
           append = TRUE, align = "center", caption = "",
           captionalign = "bottom", classcaption = "captiondataframe",
           classtable = "dataframe", digits=reportDigits, nsmall = 0,
           big.mark = "", big.interval = 3, decimal.mark = ".",
           sortableDF=FALSE)
    } else if(length(dataSet)==1){
      return(result)
    } else {
      print("------------------------------")
      print(reg.data.pages[i])
      print(result, digits=reportDigits)
    }
  }

  if(outputHTML)
  {
    HTMLEndFile()
    browseURL(paste(defaultPath, "//", fileName, ".html", sep=""),
             browser=getOption("browser"))
  }
}


################################################################################
## ANOVA ##
###########

compareAnova <- function(dataSet=NULL, outputHTML=FALSE, defaultPath=NULL,
                         fileName=NULL, htmlTitle="default",
                         compareTitle="default", reportDigits=15){

  if(outputHTML & (is.null(defaultPath) | is.null(fileName)))
    stop("defaultPath and fileName must be defined if HTML ouput is desired")
  if(htmlTitle=="default") htmlTitle <- "ANOVA NIST Comparison"
  if(compareTitle=="default") compareTitle <- "ANOVA NIST Comparison"

  if(outputHTML)
  {
    outFile <- HTMLInitFile(outdir=defaultPath, filename=fileName,
               extension="html", HTMLframe=FALSE, BackGroundColor = "FFFFFF",
               BackGroundImg = "", Title = htmlTitle,
               CSSFile="R2HTML.css", useLaTeX=TRUE, useGrid=TRUE)
    HTML(paste("<H1>", compareTitle, "</H1>"))
    HTML("<B>Notes:</B>")
    HTML("<MENU>")
    HTML("<LI>Differences are calculated as (NIST Standard - R Result):")
    HTML("<LI>LRE (Log Relative Error) gives the approximate number of significant digits to which the values agree.  Inf indicates complete agreement.  Negative numbers indicate disagreement.")
    HTML("<LI>Values are rounded to 15 significant digits as in the NIST data set")
    HTML("<LI>SSBT / SSWT = Sums of Squares Between / Within Treatment")
    HTML("<LI>MSBT / MSWT = Mean Squares Between / Within Treatment")
    HTML("<LI>df BT / df WT = Degrees of Freedom Between / Within Treatment")
    HTML("</MENU>")
  }

  # path to data set
  std.data.base <- "http://www.itl.nist.gov/div898/strd"
  anov.data.base <- paste(std.data.base, "/anova", sep="")
  anov.data.pages <- c("SiRstv.dat", "SmLs01.dat", "SmLs02.dat",
                       "SmLs03.dat", "AtmWtAg.dat", "SmLs04.dat",
                       "SmLs05.dat", "SmLs06.dat", "SmLs07.dat",
                       "SmLs08.dat", "SmLs09.dat")

  if(!is.null(dataSet))
  {
    ind <- which(anov.data.pages %in% dataSet)
    if(length(ind)==0 | length(ind)!=length(dataSet))
      stop("Selected data set(s) is(are) not one of the options")
    anov.data.pages <- anov.data.pages[ind]
  }
  anov.data <- paste(anov.data.base, anov.data.pages, sep="/")

  ##############################################################################

  for(i in 1:length(anov.data))
  {
    # Read data
    filePath <- url(anov.data[i])
    Afile <- readLines(con=filePath, n=100, ok=TRUE)
    ind.cert.val <- grep("Certified Values[[:blank:]]*[[:punct:]]lines", Afile)
    ind.data <- grep("Data[[:blank:]]*[[:punct:]]lines", Afile)
    ind.cert.val <- as.numeric(unlist(strapply(Afile[ind.cert.val], "[0-9]+")))
    ind.data <- as.numeric(unlist(strapply(Afile[ind.data], "[0-9]+")))

    if(anov.data.pages[i]=="AtmWtAg.dat") ind.cert.val <- ind.cert.val + 1

    reg.NIST <- as.numeric(unlist(strsplit(Afile[ind.cert.val[1]],
                                                 "[[:blank:]]+"))[3:6])
    res.NIST <- as.numeric(unlist(strsplit(Afile[ind.cert.val[1] + 1],
                                                 "[[:blank:]]+"))[3:5])
    r2.NIST <- as.numeric(unlist(strsplit(Afile[ind.cert.val[1]+3],
                                                "[[:blank:]]+"))[4])
    e.sd.NIST <- as.numeric(unlist(strsplit(Afile[ind.cert.val[1]+6],
                                                 "[[:blank:]]+"))[4])

    A <- read.table(filePath, skip=(ind.data[1]-1), strip.white=TRUE,
                    colClasses=c("factor","numeric"))
    lm.data <- lm(V2~V1, A)

    sum.lm.data <- summary(lm.data)
    anov.lm.data <- anova(lm.data)
    sig <- sum.lm.data$sigma
    r2 <- sum.lm.data$r.squared
    reg <- c(anov.lm.data$Df[1], anov.lm.data$Sum[1], anov.lm.data$Mean[1],
             sum.lm.data$fstatistic[1])
    res <- c(anov.lm.data$Df[2], anov.lm.data$Sum[2], anov.lm.data$Mean[2])

    result <- data.frame(matrix(nrow=9, ncol=4))
    result[,1] <- c(e.sd.NIST, r2.NIST, reg.NIST, res.NIST)
    result[,2] <- c(sig, r2, reg, res)
    result[,3] <- result[,1] - result[,2]
    result[,4] <- LRE.mod(result[,2], result[,1])
    names(result) <- c("NIST Value", "R Value", "Difference", "LRE")
    rownames(result) <- c("Residual Std Dev", "R^2", "df BT", "SSBT", "MSBT", "F",
                          "df WT", "SSWT", "MSWT")
    result <- signif(result, digits=15)

    if(outputHTML)
    {
      HTML(paste("Dataset:<B>", anov.data.pages[i], "</B>"))
      HTML(result, Border = 1, innerBorder=1, classfirstline = "firstline",
           classfirstcolumn = "firstcolumn", classcellinside = "cellinside",
           append = TRUE, align = "center", caption = "",
           captionalign = "bottom", classcaption = "captiondataframe",
           classtable = "dataframe", digits=reportDigits, nsmall = 0,
           big.mark = "", big.interval = 3, decimal.mark = ".",
           sortableDF=FALSE)
    } else if(length(dataSet)==1){
      return(result)
    } else {
      print("------------------------------")
      print(anov.data.pages[i])
      print(result, digits=reportDigits)
    }
  }

  if(outputHTML)
  {
    HTMLEndFile()
    browseURL(paste(defaultPath, "//", fileName, ".html", sep=""),
             browser=getOption("browser"))
  }
}













