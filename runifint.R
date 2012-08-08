################################################################################
#
# Function:  runifint.R
# Purpose:   To return a vector of length N of random integers between A and B
# Author:    Rob Carnell
# Created:   May 05
#
# Variables:
#  N = length of vector of integers to return
#  A = minimum number
#  B = maximum number
#
# Discussion:
#  If A or B are not integers then the function uses the integers in the
#  interval formed by A and B.  Negative numbers are allowed.
#
################################################################################

runifint <- function(N=1, A=0, B=1)
{
  if(A>B) {
    stop("A must be less than B\n")
  }
  else if(A==B & A==ceiling(A)) return(A)
  else if(A==B & A!=ceiling(A))
    stop("No integers exist in that interval\n")
  else if(ceiling(A)==floor(B)) return(ceiling(A))
  else {
    A <- ceiling(A)
    B <- floor(B)
    r <- runif(N, min=0, max=1)
    int <- A + floor(r*(B+1-A))
    int[which(int>B)] <- B
    return(int)
  }
}



