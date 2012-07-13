### Class definition

setClass("rational",
  representation=list(numerator="numeric", denominator="numeric",
                      value="numeric"),
  valid=isValidRational
)

setMethod("initialize",
  "rational",
  function(.Object, numerator=numeric(0), denominator=numeric(0))
  {
    .Object@numerator <- numerator
    .Object@denominator <- denominator
    .Object@value = .Object@numerator / .Object@denominator
    # validity checks on default initialize which is overridden here
    #  so call default initialize with callNextMethod
    callNextMethod(.Object=.Object, numerator=numerator, denominator=denominator)
  }
)

isValidRational <- function(object)
{
  if(length(object@numerator) == length(object@denominator))
  {
    if(abs(object@numerator) <= .Machine$integer.max &&
       abs(object@denominator) <= .Machine$integer.max &&
       floor(object@numerator) == object@numerator &&
       floor(object@denominator) == object@denominator)
    {
      return(TRUE)
    } else
    {
      return("\nValid rational numbers have integers in the numerator and denominator")
    }
  } else
  {
    return("\nNumerator and denominator must have equal length")
  }
}

setMethod("length",
  "rational",
  function(x)
  {
    callNextMethod(x@numerator)
  }
)

setMethod("[",
  "rational",
  function(x, i, j, ..., drop)
  {
    rational(x@numerator[i], x@denominator[i])
  }
)

### Helper functions

# http://tolstoy.newcastle.edu.au/R/e2/help/07/04/14709.html
gcd <- function(a,b) ifelse (b==0, a, gcd(b, a %% b))

rational <- function(numerator, denominator)
{
  new("rational", numerator, denominator)
}

int <- function(x)
{
  as.integer(x)
}

setGeneric("setNumerator",
  def=function(x, newNumerator)
  {
    standardGeneric("setNumerator")
  }
)

setMethod("setNumerator", signature=c("rational","numeric"),
  function(x, newNumerator)
  {
    d <- x@denominator
    rational(newNumerator, d)
  }
)

setGeneric("setDenominator",
  def=function(x, newDenominator)
  {
    standardGeneric("setDenominator")
  }
)

setMethod("setDenominator", signature=c("rational","numeric"),
  function(x, newDenominator)
  {
    n <- x@numerator
    rational(n, newDenominator)
  }
)

################################################################################
# Arith group

setMethod("Arith", signature=c("rational", "numeric"),
  function(e1,e2)
  {
    if (!is.integer(e2))
    {
      callGeneric(e1@value, e2)
    } else
    {
      # call the method with "rational", "rational" as the signature
      callNextMethod(e1, rational(e2, 1))
    }
  }
)

setMethod("Arith", signature=c("numeric", "rational"),
  function(e1,e2)
  {
    if (!is.integer(e1))
    {
      callGeneric(e1, e2@value)
    } else
    {
      # call the method with "rational", "rational" as the signature
      callNextMethod(rational(e1, 1), e2)
    }
  }
)

setMethod("+", signature=c("rational", "rational"),
  function(e1,e2)
  {
    n <- e1@numerator*e2@denominator + e2@numerator*e1@denominator
    d <- e1@denominator * e2@denominator
    g <- gcd(n,d)
    rational(n%/%g, d%/%g)
  }
)

setMethod("-", signature=c("rational", "rational"),
  function(e1,e2)
  {
    n <- e1@numerator*e2@denominator - e2@numerator*e1@denominator
    d <- e1@denominator * e2@denominator
    rational(n, d)
  }
)

setMethod("*", signature=c("rational", "rational"),
  function(e1,e2)
  {
    n <- e1@numerator * e2@numerator
    d <- e1@denominator * e2@denominator
    rational(n, d)
  }
)

setMethod("/", signature=c("rational", "rational"),
  function(e1,e2)
  {
    n <- e1@numerator * e2@denominator
    d <- e1@denominator * e2@numerator
    rational(n, d)
  }
)

setMethod("^", signature=c("rational", "rational"),
  function(e1,e2)
  {
    if (e2@denominator == 1)
    {
      n <- e1@numerator ^ e2@numerator
      d <- e1@denominator ^ e2@numerator
      rational(n, d)
    } else
    {
      callGeneric(e1@value, e2@value)
    }
  }
)

setMethod("%/%", signature=c("rational","rational"),
  function(e1,e2)
  {
    rational(callGeneric(e1@value, e2@value), 1)
  }
)

setMethod("%%", signature=c("rational", "rational"),
  function(e1,e2)
  {
    e1 - e2 * e1 %/% e2
  }
)

################################################################################

setMethod("Compare", signature=c("rational", "numeric"),
  function(e1, e2)
  {
    if (!is.integer(e2))
    {
      callGeneric(e1@value, e2)
    } else
    {
      # call the compare method with "rational", "rational" as the signature
      callNextMethod(e1, rational(e2, 1))
    }
  }
)

setMethod("Compare", signature=c("numeric", "rational"),
  function(e1, e2)
  {
    if (!is.integer(e1))
    {
      callGeneric(e1, e2@value)
    } else
    {
      # call the compare method with "rational", "rational" as the signature
      callNextMethod(rational(e1, 1), e2)
    }
  }
)

setMethod("Compare", signature=c("rational", "rational"),
  function(e1, e2)
  {
    if (e1@denominator == e2@denominator)
    {
      callGeneric(e1@numerator, e2@numerator)
    } else
    {
      # give numbers the same denominator to compare
      n1 <- e1@numerator * e2@denominator
      n2 <- e2@numerator * e1@denominator
      callGeneric(n1, n2)
    }
  }
)


################################################################################

setMethod("Math", signature=c("rational"),
  function(x)
  {
    callGeneric(x@value)
  }
)

setMethod("Math2", signature=c("rational"),
  function(x)
  {
    callGeneric(x@value)
  }
)

setMethod("abs", signature=c("rational"),
  function(x)
  {
    n <- abs(x@numerator)
    d <- abs(x@denominator)
    rational(n, d)
  }
)

setMethod("log", signature=c("rational"),
  function(x)
  {
    log(x@numerator) - log(x@denominator)
  }
)

setMethod("log10", signature=c("rational"),
  function(x)
  {
    log10(x@numerator) - log10(x@denominator)
  }
)

setGeneric("logb")

setMethod("logb", signature=c("rational"),
  function(x, base)
  {
    logb(x@numerator, base=base)-logb(x@denominator, base=base)
  }
)

setMethod("log2", signature=c("rational"),
  function(x)
  {
    log2(x@numerator) - log2(x@denominator)
  }
)

setMethod("gamma", signature=c("rational"),
  function(x)
  {
    if (x@denominator == 2 && x@numerator %% 2 == 1)
    {
      # gamma(n+1/2) = choose(n-1/2,n)n!sqrt(pi)
      if (sign(x@numerator) == 1)
      {
        n <- (x@numerator - 1) %/% 2
        choose(n-0.5, n)*factorial(n)*sqrt(pi)
      } else
      {
        n <- -(x@numerator -1) %/% 2
        sqrt(pi)/choose(-0.5, n)/factorial(n)

      }
    } else
    {
      gamma(x@value)
    }
  }
)

################################################################################

# Summary

setMethod("max", signature=c("rational"),
  function(x, na.rm=FALSE)
  {
    m <- which.max(x@value)
    rational(x@numerator[m], x@denominator[m])
  }
)

setMethod("min", signature=c("rational"),
  function(x, na.rm=FALSE)
  {
    m <- which.min(x@value)
    rational(x@numerator[m], x@denominator[m])
  }
)

setMethod("range", signature=c("rational"),
  function(x, na.rm=FALSE)
  {
    c(min(x), max(x))
  }
)

setMethod("prod", signature=c("rational"),
  function(x, na.rm=FALSE)
  {
    n <- prod(x@numerator)
    d <- prod(x@denominator)
    if (n > .Machine$integer.max || d > .Machine$integer.max)
    {
      warning("Numerator or denominator are too large, converting to numeric")
      return(n/d)
    } else
    {
      return(rational(n, d))
    }
  }
)

setMethod("sum", signature=c("rational"),
  function(x, na.rm=FALSE)
  {
    d <- prod(x@denominator)
    n <- sum(x@numerator * d %/% x@denominator)
    if (n > .Machine$integer.max || d > .Machine$integer.max)
    {
      warning("Numerator or denominator are too large, converting to numeric")
      return(n/d)
    } else
    {
      g <- (n, d)
      return(rational(n%/%g, d%/%g))
    }
  }
)

# Complex - not done


################################################################################

setMethod("print", signature="rational",
  function(x)
  {
    print(paste(x@numerator, "/", x@denominator, "=", x@value))
  }
)

setMethod("show", signature="rational",
  function(object)
  {
    print(paste(object@numerator, "/", object@denominator, "=", object@value))
  }
)

################################################################################

setMethod("as.numeric", signature="rational",
  function(x,...)
  {
    x@value
  }
)

setMethod("as.integer", signature="rational",
  function(x,...)
  {
    if (x@denominator == 1)
    {
      int(x@numerator)
    } else
    {
      int(x@value)
    }
  }
)

setMethod("as.character", signature="rational",
  function(x,...)
  {
    paste(x@numerator, "/", x@denominator)
  }
)

setGeneric("as.rational",
  def=function(x, cycles=10, max.denominator=2000)
  {
    standardGeneric("as.rational")
  }
)

setMethod("as.rational", signature="numeric",
  function(x, cycles=10, max.denominator=2000)
  {
    if (!is.integer(x))
    {
      r <- MASS:::.rat(x, cycles, max.denominator)$rat
      if (length(x) == 1)
      {
        if (r[1]/r[2] != x)
        {
          warning("as.rational produced an approximate rational number")
        }
        structure(.Data=rational(r[1], r[2]), abs.error=abs(r[1]/r[2]-x),
          class=c("rational", "numeric"))
      } else if (length(x) > 1)
      {
        if (any(r[,1]/r[,2] != x))
        {
          warning("as.rational produced an approximate rational number")
        }
        structure(.Data=rational(r[,1], r[,2]), abs.error=abs(r[,1]/r[,2]-x),
          class=c("rational", "numeric"))
      }
    } else
    {
      rational(x, 1)
    }
  }
)

setMethod("as.rational", signature="character",
  function(x, cycles=10, max.denominator=2000)
  {
    b <- as.numeric(x)
    as.rational(b, cycles, max.denominator)
  }
)

################################################################################

a <- rational(4, 5)
b <- rational(2, 3)

a + b
a - b
a * b
a / b
a %/% b
rational(25,5) %/% rational(4,1)
rational(80,5) %/% rational(7,2)
a %% b
rational(5,1) %/% rational(2,1)


a + 3
a - 3
a * 3
a / 3
a %/% 2
a %% 2

a + int(3)
a - int(3)
a * int(3)
a / int(3)
a %/% int(2)
a %% int(2)

a^b
a^3
a^int(3)


a>b
a<b
a>=b
a<=b
a==b
a!=b

a>0.8
a<0.8
a>=0.8
a<=0.8
a==0.8
a!=0.8

a>int(1)
a<int(1)
a>=int(1)
a<=int(1)
a==int(1)
a!=int(1)

rational(1,1)==int(1)
rational(1,1)==1

a <- rational(-1,2)

abs(a)
sign(a)

a <- rational(4, 5)
sqrt(a)
ceiling(a)
floor(a)
trunc(a)

log(a)
log10(a)
log2(a)
log1p(a)

logb(a, base=8)

cummax(a)
cummin(a)
cumprod(a)
cumsum(a)

acos(a)
acosh(a)
asin(a)
asinh(a)
atan(a)
atanh(a)
cos(a)
cosh(a)
sin(a)
sinh(a)
tan(a)
tanh(a)

exp(a)
expm1(a)

gamma(a)
lgamma(a)
digamma(a)
trigamma(a)

gamma(rational(1,2))
gamma(0.5)
gamma(rational(3,2))
gamma(1.5)
gamma(rational(5,2))
gamma(2.5)
gamma(rational(7,2))
gamma(3.5)

a <- rational(1, 2)
b <- rational(2, 4)

a + b
a == b

as.numeric(a)
as.integer(a)
as.integer(rational(2,1))
as.integer(rational(3,2))
as.integer(rational(4345,32))
as.character(rational(2,1))
as.character(rational(3,2))
as.character(rational(4345,32))


rational(c(1,2),c(3,4))
rational(5,6)

a <- rational(c(1,2,3),c(4,5,6))

max(a)
min(a)
range(a)
sum(a)
prod(a)

0.1+0.1+0.1==0.3
sum(rational(c(1,1,1),c(10,10,10))) == 0.3
sum(rational(c(1,1,1),c(10,10,10))) == rational(3,10)

a <- as.rational(0.3)
a
b <- as.rational(c(0.3,.3333))
b

attr(a, "abs.error")
attr(b, "abs.error")

a <- as.rational(0.3333, cycles=20, max.denominator=100000)
attr(a, "abs.error")

a <- as.rational("0.3")
a

