# chunks for the rational_class.Rmd file

## ---- setupchunk
require(RUnit)
require(R6)

# http://tolstoy.newcastle.edu.au/R/e2/help/07/04/14709.html
# greatest common denominator
gcd <- function(a,b) ifelse (b==0, a, gcd(b, a %% b))

rationalAddRational <- function(n1, d1, n2, d2)
{
  n <- n1*d2 + n2*d1
  d <- d1*d2
  g <- gcd(n,d)
  n <- n %/% g
  d <- d %/% g
  return(list(n, d))
}

rationalAddInteger <- function(n1, d1, i2) rationalAddRational(n1, d1, i2, rep(1, length(i2)))
rationalAddNumeric <- function(n1, d1, f2) n1 / d1 + f2


## ---- R6class
R6rational <- R6Class(
  classname="rational",
  private=list(numerator=integer(1), denominator=integer(1), value=numeric(1)),
  public=list(
    getNumerator=function() return(private$numerator),
    getDenominator=function() return(private$denominator),
    getValue=function() return(private$value),
    reciprocal=function()
    {
      temp <- private$denominator
      private$denominator <- private$numerator
      private$numerator <- temp
    },
    setNumerator=function(numerator)
    {
      if (any(!is.finite(numerator)))
        stop("the rational class cannot contain NA, NaN, or Inf")
      if (length(numerator) != length(private$denominator))
        stop("Numerator and denominator must have equal length")
      if (any(!is.integer(numerator)))
      {
        stop("the rational class must be instantiated with integers")
      } 
      private$numerator <- numerator
      private$value = private$numerator / private$denominator
    },
    setDenominator=function(denominator)
    {
      if (any(!is.finite(denominator)))
        stop("the rational class cannot contain NA, NaN, or Inf")
      if (any(denominator == 0))
        stop("the rational class cannot include division by 0")
      if (length(private$numerator) != length(denominator))
        stop("Numerator and denominator must have equal length")
      if (any(!is.integer(denominator)))
      {
        stop("the rational class must be instantiated with integers")
      } 
      private$denominator <- denominator
      private$value = private$numerator / private$denominator
      
    },
    initialize=function(numerator, denominator)
    {
      if (any(!is.finite(numerator)) || any(!is.finite(denominator)))
        stop("the rational class cannot contain NA, NaN, or Inf")
      if (any(denominator == 0))
        stop("the rational class cannot include division by 0")
      if (length(numerator) != length(denominator))
        stop("Numerator and denominator must have equal length")
      if (any(!is.integer(numerator)) || any(!is.integer(denominator)))
      {
        stop("the rational class must be instantiated with integers")
      } 
      private$numerator <- numerator
      private$denominator <- denominator
      private$value = private$numerator / private$denominator
    },
    add=function(x)
    {
      if (is.rational(x))
      {
        rat <- rationalAddRational(private$numerator, private$denominator, 
                                   x$getNumerator(), x$getDenominator())
      } else 
      {
        rat <- rationalAddInteger(private$numerator, private$denominator, 
                                   as.integer(x))
      }
      private$numerator <- rat$n
      private$denominator <- rat$d
      private$value <- private$numerator / private$denominator
    },
    minus=function(x)
    {
    }
  )
)

a <- R6rational$new(as.integer(c(1,2,3)), as.integer(c(4,5,6)))
a$getNumerator()
a$getDenominator()
a$getValue()
a <- R6rational$new(c(1.1,2,3), c(4,5,6))
a <- R6rational$new(as.integer(c(NA,2,3)), as.integer(c(4,5,6)))
a <- R6rational$new(c(1,2,3), c(4,5,NA))
a <- R6rational$new(c(1,Inf,3), c(4,5,6))
a <- R6rational$new(c(1,2,3), c(4,-Inf,6))


## ---- test1
test1 <- function()
{
  # NA's are not permitted
  checkException(R6rational$new(as.integer(NA), as.integer(3)), silent=TRUE)
  checkException(R6rational$new(as.integer(3), as.integer(NA)), silent=TRUE)
  # the numerator and denominator must be of equal length
  checkException(R6rational$new(as.integer(c(2,3)), as.integer(c(4,5,6))), silent=TRUE)
  # NULL is not permitted
  checkException(R6rational$new(as.integer(3), NULL), silent=TRUE)
  # numerics are not permitted where integers are required
  checkException(R6rational$new(3, 4), silent=TRUE)
  # other objects like characters are not permitted
  checkException(R6rational$new("a", as.integer(4)), silent=TRUE)
}
test1()

numericErrorMessage <- paste("Numerator or Denominator cannot be represented by ",
                             "integers on [", -1*.Machine$integer.max, ",",
                             .Machine$integer.max, "]", sep="")

rational <- function(numerator, denominator)
{
  if (any(abs(numerator) > .Machine$integer.max) || 
        any(abs(denominator) > .Machine$integer.max))
  {
    stop(numericErrorMessage)
  }
  return(R6rational$new(as.integer(numerator), as.integer(denominator)))
}

is.rational <- function(x) {
  if (any(class(x) == "rational")) return(TRUE) 
  else return(FALSE)
}

## ---- test2
test2 <- function()
{
  a <- R6rational$new(as.integer(3), as.integer(4))
  b <- rational(as.integer(3), as.integer(4))
  checkTrue(a$getValue() == b$getValue() && 
              a$getNumerator() == b$getNumerator() && 
            a$getDenominator() == b$getDenominator())
  b <- rational(3,4)
  checkTrue(a$getValue() == b$getValue() && 
              a$getNumerator() == b$getNumerator() && 
              a$getDenominator() == b$getDenominator())
  b <- rational(3, as.integer(4))
  checkTrue(a$getValue() == b$getValue() && 
              a$getNumerator() == b$getNumerator() && 
              a$getDenominator() == b$getDenominator())
  b <- rational(as.integer(3), 4)
  checkTrue(a$getValue() == b$getValue() && 
              a$getNumerator() == b$getNumerator() && 
              a$getDenominator() == b$getDenominator())
  
  checkException(rational(as.integer(NA), as.integer(3)), silent=TRUE)
  checkException(rational(as.integer(3), as.integer(NA)), silent=TRUE)
  checkException(rational(as.integer(c(2,3)), as.integer(c(4,5,6))), silent=TRUE)
  checkException(rational(as.integer(3), NULL), silent=TRUE)
  checkException(rational("a", as.integer(4)), silent=TRUE)
}
test2()

length.rational <- function(x)
{
  return(length(x$getNumerator()))
}
a <- rational(c(1,2), c(3,4))
length(a)

'[.rational' <- function(x,i,..., drop=TRUE)
{
  return(rational(x$getNumerator()[i], x$getDenominator()[i]))
}

getS3method("length", "rational")
getS3method("[", "rational")


## ---- test3
test3 <- function()
{
  a <- rational(3, 4)
  checkEquals(length(a), 1)
  a <- rational(c(3,4), c(5,6))
  checkEquals(length(a), 2)
  
  checkEquals(a[1]$getValue(), 0.6)
  checkEquals(a[2]$getValue(), 4/6)
  checkEquals(a[1:2]$getValue(), c(0.6, 4/6))
}
test3()


## ---- test4
test4 <- function()
{
  a <- rational(1,2)
  checkEquals(a$getNumerator(), 1)
  checkEquals(a$getDenominator(), 2)
  checkEquals(a$getValue(), 0.5)
  a$setNumerator(as.integer(3))
  checkEquals(a$getValue(), 1.5)
  checkEquals(a$getNumerator(), 3)
  a$setDenominator(as.integer(5))
  checkEquals(a$getValue(), 0.6)
  checkEquals(a$getDenominator(), 5)
}
test4()

'+.rational' <- function(e1, e2)
{
  if (is.rational(e1) && is.rational(e2))
  {
    n <- e1$getNumerator()*e2$getDenominator() + e2$getNumerator()*e1$getDenominator()
    d <- e1$getDenominator()*e2$getDenominator()
  } else if (is.rational(e1))
  {
    n <- e1$getNumerator() + as.integer(e2)*e1$getDenominator()
    d <- e1$getDenominator()
  } else if (is.rational(e2))
  {
    n <- as.integer(e1)*e2$getDenominator() + e2$getNumerator()
    d <- e2$getDenominator()
  } else stop("unrecognized")
  g <- gcd(n,d)
  return(rational(n%/%g, d%/%g))
}

'-.rational' <- function(e1, e2)
{
  if (is.rational(e1) && is.rational(e2))
  {
    n <- e1$getNumerator()*e2$getDenominator() - e2$getNumerator()*e1$getDenominator()
    d <- e1$getDenominator()*e2$getDenominator()
  } else if (is.rational(e1))
  {
    n <- e1$getNumerator() - as.integer(e2)*e1$getDenominator()
    d <- e1$getDenominator()
  } else if (is.rational(e2))
  {
    n <- as.integer(e1)*e2$getDenominator() - e2$getNumerator()
    d <- e2$getDenominator()
  } else stop("unrecognized")
  g <- gcd(n,d)
  return(rational(n%/%g, d%/%g))
}

'*.rational' <- function(e1, e2)
{
  if (is.rational(e1) && is.rational(e2))
  {
    n <- e1$getNumerator() * e2$getNumerator()
    d <- e1$getDenominator() * e2$getDenominator()
  } else if (is.rational(e1))
  {
    n <- e1$getNumerator() * as.integer(e2)
    d <- e1$getDenominator()
  } else if (is.rational(e2))
  {
    n <- e2$getNumerator() * as.integer(e1)
    d <- e2$getDenominator()
  } else stop("unrecognized")
  return(rational(n, d))
}

'/.rational' <- function(e1, e2)
{
  if (is.rational(e1) && is.rational(e2))
  {
    n <- e1$getNumerator() * e2$getDenominator()
    d <- e1$getDenominator() * e2$getNumerator()
  } else if (is.rational(e1))
  {
    n <- e1$getNumerator() 
    d <- e1$getDenominator() * as.integer(e2)
  } else if (is.rational(e2))
  {
    n <- as.integer(e1) * e2$getDenominator()
    d <- e2$getNumerator()
  } else stop("unrecognized")
  return(rational(n, d))
}

'^.rational' <- function(e1, e2)
{
  if (is.rational(e1) && is.rational(e2))
  {
    if (e2$getDenominator() == 1)
    {
      n <- e1$getNumerator() ^ e2$getNumerator()
      d <- e1$getDenominator() ^ e2$getNumerator()
      return(rational(n, d))
    } else
    {
      return(e1$getValue()^e2$getValue())
    }
  } else if (is.rational(e1))
  {
    n <- e1$getNumerator() ^ as.integer(e2)
    d <- e1$getDenominator() ^ as.integer(e2)
    return(rational(n, d))
  } else if (is.rational(e2))
  {
    return(e1^e2$getValue())
  } else stop("unrecognized")
}

'%/%.rational' <- function(e1, e2)
{
  if (is.rational(e1) && is.rational(e2))
  {
    n1 <- e1$getNumerator() * e2$getDenominator()
    n2 <- e2$getNumerator() * e1$getDenominator()
    return(n1 %/% n2)
  } else if (is.rational(e1))
  {
    return(e1$getValue() %/% e2)
  } else if (is.rational(e2))
  {
    return(e1 %/% e2$getValue())
  } else stop("unrecognized")
}

'%%.rational' <- function(e1, e2)
{
  if (is.rational(e1) && is.rational(e2))
  {
    e1 - e2 * e1 %/% e2
  } else if (is.rational(e1))
  {
    return(e1$getValue() %% e2)
  } else if (is.rational(e2))
  {
    return(e1 %% e2$getValue())
  } else stop("unrecognized")
}


## ---- test5
test5 <- function()
{
  a <- rational(4, 5)
  b <- rational(2, 3)
  
  d <- a + b
  checkEquals(d$getValue(), 4/5+2/3)
  d <- a - b
  checkEquals(d$getValue(), 4/5-2/3)
  d <- a * b
  checkEquals(d$getValue(), (4/5)*(2/3))
  d <- a / b
  checkEquals(d$getValue(), (4/5) / (2/3))
  d <- a %/% b
  checkEquals(d, 1)
  checkEquals(rational(5,3) %/% rational(2,3), 2)
  checkEquals(rational(5,4) %/% rational(3,5), 2)
  checkEquals(rational(25,5) %/% rational(4,1), 1)
  checkEquals(rational(80,5) %/% rational(7,2), 4)
  checkEquals(rational(5,1) %/% rational(2,1), 2)
  d <- a %% b
  checkEquals(d$getValue(), 2/15)
    
  d <- a + 3
  checkEquals(d$getValue(), (4/5)+3)
  d <- a + rational(3,1)
  checkEquals(d$getValue(), (4/5)+3)
  ############################################## change
  #d <- a + 3.3
  #checkEquals(d, (4/5) + 3.3)
  d <- a + 3.3
  checkEquals(d$getValue(), (4/5) + 3)
  ###############################################
  d <- a - 3
  checkEquals(d$getValue(), (4/5)-3)
  d <- a * 3
  checkEquals(d$getValue(), (4/5)*3)
  d <- a / 3
  checkEquals(d$getValue(), (4/5)/3)
  d <- a %/% 2
  checkEquals(d, 0)
  ########################################### change
  d <- a %% 2
  checkEquals(a$getValue(), d)
  #############################################
  
  d <- a + as.integer(3)
  checkEquals(d$getValue(), (4/5)+3)
  d <- a - as.integer(3)
  checkEquals(d$getValue(), (4/5)-3)
  d <- a * as.integer(3)
  checkEquals(d$getValue(), (4/5)*3)
  d <- a / as.integer(3)
  checkEquals(d$getValue(), (4/5)/3)
  d <- a %/% as.integer(2)
  checkEquals(d, 0)
  ########################################## change
  d <- a %% as.integer(2)
  #checkEquals(a@value, d@value)
  checkEquals(a$getValue(), d)
  ##########################################
  
  d <- a^b
  checkEquals(d, (4/5)^(2/3))
  d <- a^3
  checkEquals(d$getValue(), 4*4*4/5/5/5)
  d <- a^as.integer(3)
  checkEquals(d$getValue(), 4*4*4/5/5/5)


  
  a <- rational(4, 5)
  b <- rational(2, 3)
  a$add(b)
  checkEquals(a$getValue(), 4/5+2/3)
}
test5()



## ---- compare
setMethod("Compare", signature=c("rational", "numeric"),
  function(e1, e2)
  {
    if (e2 != as.integer(e2))
    {
      callGeneric(e1@value, e2)
    } else
    {
      callGeneric(e1@numerator, e2*e1@denominator)
    }
  }
)

setMethod("Compare", signature=c("rational", "integer"),
  function(e1, e2)
  {
    callGeneric(e1@numerator, e2*e1@denominator)
  }
)

setMethod("Compare", signature=c("numeric", "rational"),
  function(e1, e2)
  {
    if (e1 != as.integer(e1))
    {
      callGeneric(e1, e2@value)
    } else
    {
      callGeneric(e1*e2@denominator, e2@numerator)
    }
  }
)

setMethod("Compare", signature=c("integer", "rational"),
  function(e1, e2)
  {
    callGeneric(e1*e2@denominator, e2@numerator)
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

## ---- test6
test6 <- function()
{
  a <- rational(4, 5)
  b <- rational(2, 3)
  checkTrue(a>b)
  checkTrue(!(a<b))
  checkTrue(a>=b)
  checkTrue(!(a<=b))
  checkTrue(!(a==b))
  checkTrue(a!=b)
  
  checkTrue(!(a>0.8))
  checkTrue(!(a<0.8))
  checkTrue(a>=0.8)
  checkTrue(a<=0.8)
  checkTrue(a==0.8)
  checkTrue(!(a!=0.8))
  
  checkTrue(!(a>2))
  checkTrue(a<2)
  checkTrue(!(a>=2))
  checkTrue(a<=2)
  checkTrue(!(a==2))
  checkTrue(a!=2)

  checkTrue(!(a>as.integer(1)))
  checkTrue(a<as.integer(1))
  checkTrue(!(a>=as.integer(1)))
  checkTrue(a<=as.integer(1))
  checkTrue(!(a==as.integer(1)))
  checkTrue(a!=as.integer(1))
  
  checkTrue(rational(1,1)==as.integer(1))
  checkTrue(rational(1,1)==1)
}
test6()

## ---- math
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

## ---- test7
test7 <- function()
{
  a <- rational(-1,2)
  
  d <- abs(a)
  checkEquals(d@value, 0.5)
  checkEquals(sign(a), -1)
  
  a <- rational(4, 5)
  b <- rational(3, 2)
  checkEquals(sqrt(a), sqrt(4/5))
  checkEquals(ceiling(a), 1)
  checkEquals(floor(a), 0)
  checkEquals(trunc(a), 0)
  
  checkEquals(log(a), log(4/5))
  checkEquals(log10(a), log10(4/5))
  checkEquals(log2(a), log2(4/5))
  checkEquals(log1p(a), log1p(4/5))
  checkEquals(logb(a, base=8), logb(4/5, base=8))
  
  checkEquals(acos(a), acos(4/5))
  checkEquals(acosh(b), acosh(3/2))
  checkEquals(asin(a), asin(4/5))
  checkEquals(asinh(a), asinh(4/5))
  checkEquals(atan(a), atan(4/5))
  checkEquals(atanh(a), atanh(4/5))
  checkEquals(cos(a), cos(4/5))
  checkEquals(cosh(a), cosh(4/5))
  checkEquals(sin(a), sin(4/5))
  checkEquals(sinh(a), sinh(4/5))
  checkEquals(tan(a), tan(4/5))
  checkEquals(tanh(a), tanh(4/5))
  checkEquals(exp(a), exp(4/5))
  checkEquals(expm1(a), expm1(4/5))
  
  checkEquals(gamma(a), gamma(4/5))
  checkEquals(lgamma(a), lgamma(4/5))
  checkEquals(digamma(a), digamma(4/5))
  checkEquals(trigamma(a), trigamma(4/5))
  
  checkEquals(gamma(rational(1,2)), gamma(0.5))
  checkEquals(gamma(rational(3,2)), gamma(1.5))
  checkEquals(gamma(rational(5,2)), gamma(2.5))
  checkEquals(gamma(rational(7,2)), gamma(3.5))
  
  #a <- rational(1, 2)
  #b <- rational(2, 4)
  
  #checkEquals(a + b, 1)
  #checkTrue(a == b)

  # cum functions are part of the Math group and need work
  #a <- rational(4,5)
  #b <- rational(3,4)
  #d <- rational(c(2,3,4,5), c(3,4,5,6))
  #d <- cumsum(a)
  #print(d)
  #checkEquals(d, 4/5)
  
  #cummax(a)
  #cummin(a)
  #cumprod(a)
  #cumsum(a)
}
test7()

## ---- summary
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
      g <- gcd(n, d)
      return(rational(n%/%g, d%/%g))
    }
  }
)

## ---- test8
test8 <- function()
{
  checkTrue(!(0.1+0.1+0.1==0.3))
  checkTrue(sum(rational(c(1,1,1),c(10,10,10))) == 0.3)
  checkTrue(sum(rational(c(1,1,1),c(10,10,10))) == rational(3,10))
  checkTrue(max(rational(c(2,3,4,1),c(5,7,3,2))) == rational(4,3))
  checkTrue(min(rational(c(2,3,4,1),c(5,7,3,2))) == rational(2,5))
  d <- range(rational(c(2,3,4,1),c(5,7,3,2)))
  checkEquals(d[[1]]@value, 2/5)
  checkEquals(d[[2]]@value, 4/3)
  checkTrue(d[[1]] == rational(2,5))
  checkTrue(d[[2]] == rational(4,3))
  checkTrue(prod(rational(c(1,2,3), c(10,10,10))) == rational(6,1000))
}
test8()

## ---- display
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

## ---- conversion
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
      as.integer(x@numerator)
    } else
    {
      as.integer(x@value)
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

## ---- test9
test9 <- function()
{
  a <- as.rational(0.3)
  checkTrue(a == rational(3,10))
  
  checkTrue(tryCatch(b <- as.rational(c(0.3,0.3333)), error=function(e) stop(e), warning=function(w) return(TRUE)))
  suppressWarnings(b <- as.rational(c(0.3,0.3333)))
  checkTrue(b[1] == rational(3,10))
  checkTrue(b[2] == rational(1,3))
  checkEquals(attr(a, "abs.error"), 0)
  checkEquals(attr(b, "abs.error"), c(0, 1/3-0.3333))
  
  a <- as.rational(0.3333, cycles=20, max.denominator=100000)
  checkTrue(a == rational(3333, 10000))
  checkEquals(attr(a, "abs.error"), 0)
  
  a <- as.rational("0.3")
  checkTrue(a == rational(3,10))
}
test9()
