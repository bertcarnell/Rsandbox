m1 <- -2
m2 <- -1.5
m3 <- 0
s3 <- 0.3

x <- seq(0, 2, by=0.0001)
a <- dlnorm(x, m1, 0.1)
b <- dlnorm(x, m2, 0.1)
d <- dlnorm(x, m3, s3)

plot(x, a, type="l", col=1)
points(x, b, type="l", col=2)
points(x, d, type="l", col=3)

Fx <- 1/3*plnorm(x, m1, 0.1)+1/3*plnorm(x, m2, 0.1)+1/3*plnorm(x, m3, s3)
plot(x, Fx)
m <- 1/3*exp(m1+0.1*0.1/2)+1/3*exp(m2+0.1*0.1/2)+1/3*exp(m3+s3*s3/2)
m
abline(v=m, col="red")

N <- 10000
mean(c(rlnorm(N, m1, 0.1), rlnorm(N, m2, 0.1), rlnorm(N, m3, s3)))

sum(x[1:(length(x)-1)]*diff(Fx))

#######

geom <- log(c(rlnorm(N, m1, 0.1), rlnorm(N, m2, 0.1), rlnorm(N, m3, s3)))
exp(mean(geom))

mg <- exp(1/3*(m1+0.1*0.1/2)+1/3*(m2+0.1*0.1/2)+1/3*(m3+s3*s3/2))
mg

y <- seq(0.0001, .9999, length=10000)
Y <- matrix(c(qlnorm(y,m1,0.1), qlnorm(y,m2,0.1), qlnorm(y,m3,s3)), nrow=length(y), ncol=3)
Fyg <- exp(apply(Y, 1, function(x) mean(log(x))))

sum(Fyg[1:(length(y)-1)]*diff(y))

plot(Fyg, y, col="purple", xlim=c(0,2), type="l")
points(x, Fx, col="orange", type="l")
points(x, plnorm(x,m1, 0.1), type="l")
points(x, plnorm(x,m2, 0.1), type="l")
points(x, plnorm(x,m3, s3), type="l")

plot(Fyg, y, col="purple", xlim=c(0.1,2), type="l", log="x")
points(x, Fx, col="orange", type="l")
points(x, plnorm(x, m1, 0.1), type="l")
points(x, plnorm(x, m2, 0.1), type="l")
points(x, plnorm(x, m3, s3), type="l")

Fx2 <- exp(log(plnorm(x, m1, 0.1))/3+log(plnorm(x, m2, 0.1))/3+log(plnorm(x, m3, s3))/3)
points(x, Fx2, type="l", col="blue")

temp <- 1/exp(m1)+1/exp(m2)+1/exp(m3)
Fx3 <- 1/exp(m1)/temp*plnorm(x, m1, 0.1)+1/exp(m2)/temp*plnorm(x, m2, 0.1)+1/exp(m3)/temp*plnorm(x, m3, s3)
points(x, Fx3, type="l", col="red")

sum(x[1:(length(x)-1)]*diff(Fx3))
