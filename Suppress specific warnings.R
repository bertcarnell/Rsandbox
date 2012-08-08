#The suppressWarnings function allows to call a function and 
#suppress whatever warning it might generate. This uses the calling 
#handler and restart mechanism of R to invoke the special "muffleWarning" 
#restart when a warning is detected by the handler.

#The downside is that it removes all warnings, and this might not be 
#what you want. Consider that simple function that gives two warnings: 

f <- function( x) {
  warning( "bla bla" )
  y <- x + 3
  warning( "yada yada" )
  y
}

f(5)
suppressWarnings( f(5) )

#What if I wanted to remove "bla bla" warnings and not "yada yada" warnings, 
#because I know that "bla bla" warnings are expected and are more disturbing 
#than useful. Currently, suppressWarnings does not offer that possibility, 
#but you can make you own calling handler that handles warnings the way you want: 

h <- function(w) if( any( grep( "bla", w$message) ) ) invokeRestart( "muffleWarning" )
withCallingHandlers( f(5), warning = h )

suppressWarning <- function(expr, suppressString)
{
   h <- function(w) if( any( grep( suppressString, w$message) ) ) invokeRestart( "muffleWarning" )
   withCallingHandlers(expr, warning = h)
}

f <- function( x) {
  warning( "bla bla" )
  y <- x + 3
  warning( "yada yada" )
  y
}

f(5)
suppressWarnings(f(5))
suppressWarning(f(5), "bla")
suppressWarning(f(5), "a")
suppressWarning(f(5), "g")


#### Note:  Rob changed this from the original post to make it work...