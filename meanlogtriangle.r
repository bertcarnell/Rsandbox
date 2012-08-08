require(triangle)

y <- runif(100000)
x <- 10^qtriangle(y, log10(10), log10(1000), log10(100))
hist(x, breaks=100)

mean(x)
10^((log10(10)+log10(1000)+log10(100))/3)

integrate(dtriangle, lower=1, upper=3, 1, 3, 2)

dltriangle <- function(q, a, b, c)
{
  A <- log10(a)
  B <- log10(b)
  C <- log10(c)
  lq <- q
  if (lq < A)
  {
    0
  } else if (lq >= A && lq <= C)
  {
    2/(C-A)/(B-A)*(lq-A)
  } else if (lq > C  && lq <= B)
  {
    2/(C-B)/(B-A)*(lq-B)
  } else
  {
    0
  }
}

integrate(Vectorize(dltriangle), lower=log10(10), upper=log10(1000), a=10, b=1000, c=100)

dltriangle <- function(q, a, b, c)
{
  A <- log10(a)
  B <- log10(b)
  C <- log10(c)
  lq <- log10(q)
  if (lq < A)
  {
    0
  } else if (lq >= A && lq <= C)
  {
    2/(C-A)/(B-A)*(lq-A)*abs(1/q/log(10))
  } else if (lq > C  && lq <= B)
  {
    2/(C-B)/(B-A)*(lq-B)*abs(1/q/log(10))
  } else
  {
    0
  }
}

integrate(Vectorize(dltriangle), lower=10, upper=1000, a=10, b=1000, c=100)

dmeanltriangle <- function(q, a, b, c)
{
  A <- log10(a)
  B <- log10(b)
  C <- log10(c)
  lq <- log10(q)
  if (lq < A)
  {
    0
  } else if (lq >= A && lq <= C)
  {
    2*q/(C-A)/(B-A)*(lq-A)*abs(1/q/log(10))
  } else if (lq > C  && lq <= B)
  {
    2*q/(C-B)/(B-A)*(lq-B)*abs(1/q/log(10))
  } else
  {
    0
  }
}

integrate(Vectorize(dmeanltriangle), lower=10, upper=1000, a=10, b=1000, c=100)

dmeanltriangle <- function(q, a, b, c)
{
  A <- log10(a)
  B <- log10(b)
  C <- log10(c)
  lq <- log10(q)
  if (lq < A)
  {
    0
  } else if (lq >= A && lq <= C)
  {
    2/(C-A)/(B-A)*(lq-A)/log(10)
  } else if (lq > C  && lq <= B)
  {
    2/(C-B)/(B-A)*(lq-B)/log(10)
  } else
  {
    0
  }
}

integrate(Vectorize(dmeanltriangle), lower=10, upper=1000, a=10, b=1000, c=100)
integrate(Vectorize(dmeanltriangle), lower=5, upper=5000, a=5, b=5000, c=80)


f1 <- function(a, b, c)
{
  z <- log(10)
  2/(c-a)/(b-a)/z*(log(10^c)*log(10^c)/2/z - a*log(10^c) - log(10^a)*log(10^a)/2/z + a*log(10^a))
}
f2 <- function(a, b, c)
{
  z <- log(10)
  2/(c-b)/(b-a)/z*(log(10^b)*log(10^b)/2/z - b*log(10^b) - log(10^c)*log(10^c)/2/z + b*log(10^c))
}

f1(log10(10), log10(1000), log10(100)) + f2(log10(10), log10(1000), log10(100))

f1 <- function(a, b, c)
{
  z <- log(10)
  2/(c-a)/(b-a)/z*((10^c*log(10^c)-10^c)/z - a*10^c - (10^a*log(10^a)-10^a)/z + a*10^a)
}
f2 <- function(a, b, c)
{
  z <- log(10)
  2/(c-b)/(b-a)/z*((10^b*log(10^b)-10^b)/z - b*10^b - (10^c*log(10^c)-10^c)/z + b*10^c)
}

f1(log10(10), log10(1000), log10(100)) + f2(log10(10), log10(1000), log10(100))

mean_triangle <- function(a, b, c)
{
  (a+b+c)/3
}

mean_ltriangle <- function(a, b, c)
{
  stopifnot(a>0 && b>0 && c>0)
  z <- log(10)
  la <- log10(a)
  lb <- log10(b)
  lc <- log10(c)
  part1 <- 2/(lc-la)/(lb-la)/z*((c*log(c)-c)/z - la*c - (a*log(a)-a)/z + la*a)
  part2 <- 2/(lc-lb)/(lb-la)/z*((b*log(b)-b)/z - lb*b - (c*log(c)-c)/z + lb*c)
  part1 + part2
}

mean_triangle(1,3,2) # 2
mean_ltriangle(10, 1000, 100) # 152.7755
mean_ltriangle(5, 5000, 80) # 336.649

mean_ltriangle <- function(a, b, c)
{
  # simplified and optimized
  stopifnot(a>0 && b>0 && c>0)
  z <- 1/log(10)
  la <- log10(a)
  lb <- log10(b)
  lc <- log10(c)
  coef1 <- 2*z/(lb-la)
  part1 <- coef1/(lc-la)*(c*(lc - z - la) + a*z)
  part2 <- coef1/(lc-lb)*(-b*z - c*(lc - z - lb))
  part1 + part2
}

mean_ltriangle(10, 1000, 100) # 152.7755
mean_ltriangle(5, 5000, 80) # 336.649

