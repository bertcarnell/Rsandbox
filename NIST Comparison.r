################################################################################
# Program Name:  NIST Comparison.R
# Purpose:       To compare R values with certified values from NIST
# Author:        Rob Carnell
# Date:          August 2006
#
# Input Data:    http://www.itl.nist.gov/div898/strd
# Output Data:   html output files
# Required Functions:  NIST Comparison Functions
# Required Packages: gsubfn, polynom, R2HTML
# R version:     >= 2.3.0
#
# Modified:
# Date Modified:
#
################################################################################

# default path for the HTML output
defaultPath <- file.path("c:","documents and settings","carnellr","my documents","R references")

# source the NIST Comparison Functions
source(file.path(defaultPath, "NIST Comparison Functions.R"))

# Examples of usage
compareUnivariate()
compareUnivariate(outputHTML=TRUE, defaultPath=defaultPath, fileName="Univariate")
compareUnivariate(dataSet="Lottery.dat")

compareLinRegress()
compareLinRegress(outputHTML=TRUE, defaultPath=defaultPath, fileName="Regression")
compareLinRegress(dataSet="Wampler1.dat")

compareAnova()
compareAnova(outputHTML=TRUE, defaultPath=defaultPath, fileName="Anova")
compareAnova(dataSet="SmLs01.dat")

# TODO
# check with reference on LRE interpretation and presentation of results


