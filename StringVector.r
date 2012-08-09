setClass("stringVector", representation(strng="character", reserve="numeric",
  size="numeric"), prototype=list(strng=NULL, reserve=0, size=0))
setGeneric("resize<-", function(object, value){standardGeneric("resize<-")})
setMethod("resize<-", signature(object="stringVector", value="numeric"),
  definition=function(object, value)
  {
    oldSize <- object@size
    value <- floor(value)
    if (value > oldSize)
    {
      object@strng <- c(object@strng[1:oldSize], rep(as.character(NA), value-oldSize))
      object@size <- value
    } else if (value < oldSize)
    {
      object@strng <- object@strng[1:value]
      object@size <- value
    } # else value == oldSize (do nothing)
    object
  }
)
setGeneric("reserve<-", function(object, value){standardGeneric("reserve<-")})
setMethod("reserve<-", signature(object="stringVector", value="numeric"),
  definition=function(object, value)
  {
    if (value != object@reserve && value >= object@size)
    {
      if (object@size >=1)
      {
        object@strng <- c(object@strng[1:object@size], character(value-object@size))
        object@reserve <- value
      } else
      {
        object@strng <- character(value)
        object@reserve <- value
      }
    } else if (value < object@size)
    {
      stop("Requested reserve is less than the size")
    }
    object
  }
)
setMethod("add<-", signature(object="stringVector", value="character"),
  definition=function(object, value)
  {
    oldSize <- object@size
    lenValue <- length(value)
    if (object@reserve < oldSize + lenValue)
    {
      if (lenValue > 1)
      {
        object@strng <- c(object@strng[1:oldSize], value)
        object@size <- length(object@strng)
        object@reserve <- length(object@strng)
      } else
      {
        object@strng <- c(object@strng[1:oldSize], value, character(object@size))
        object@reserve <- object@reserve + 1 + oldSize
        object@size <- oldSize + lenValue
      }
    } else #if (reserve >= size + length(value))
    {
      object@strng[(oldSize+1):(oldSize+lenValue)] <- value
      object@size <- oldSize + lenValue
    }
    object
  }
)
setGeneric("get", function(object){standardGeneric("get")})
setMethod("get", signature(object="stringVector"),
  definition=function(object)
  {
    if (object@size > 0) return(object@strng[1:object@size])
    else return(NULL)
  }
)

x <- new("stringVector")
get(x)
resize(x) <- 4
get(x)
add(x) <- "hello"
add(x) <- "Rob"
get(x)
add(x) <- LETTERS[1:5]
get(x)
resize(x) <- 6
get(x)
add(x) <- "new"
get(x)
reserve(x) <- 20
get(x)
add(x) <- "newer"
get(x)
reserve(x) <- 2

y <- new("stringVector")
system.time(for (i in 1:5000) add(y) <- as.character(i))
z <- new("stringVector")
reserve(z) <- 10000
system.time({
  for (i in 1:5000) add(z) <- as.character(i)
})
w <- NULL
system.time(for(i in 1:5000) w <- c(w, as.character(i)))
u <- character(5000)
system.time(for(i in 1:5000) u[i] <- as.character(i))
