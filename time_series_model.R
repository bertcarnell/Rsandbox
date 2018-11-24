#### Simulation Data

# X is time series measurements
# V is a random effect
# Y is a multivatiate response that is correlated and dependent on X and also a time series

TIMESTEPSPERPATIENT <- 100
NPATIENTS <- 10
N <- TIMESTEPSPERPATIENT*NPATIENTS

################################################################################

V <- rep(1:NPATIENTS, each=TIMESTEPSPERPATIENT)
times <- rep(1:TIMESTEPSPERPATIENT, times=NPATIENTS)
set.seed(1976)
X <- data.frame(
  a=0+0.25*times, #slowly increase with  time
  b=2*sin(times/TIMESTEPSPERPATIENT*4*pi), # sinusoidal
  c=rep(0:1, each=TIMESTEPSPERPATIENT, times=NPATIENTS/2), # half patients have this
  d=rnorm(N, 3, 2), # noise
  e=exp(-times/20), # die out
  f=pmax(-0.5*times^2+5+10*times, 0)+20*exp(-times/TIMESTEPSPERPATIENT*2),
  g=rbeta(N, 2, 2), # noise
  h=c(rep(1,TIMESTEPSPERPATIENT), rep(0, TIMESTEPSPERPATIENT),
      rep(1,TIMESTEPSPERPATIENT), rep(0, TIMESTEPSPERPATIENT),
      rpois(TIMESTEPSPERPATIENT, 0.1), rpois(TIMESTEPSPERPATIENT, 0.5),
      rep(0, 4*TIMESTEPSPERPATIENT)),
  i=pmax(rnorm(N, 50, 5), 0) #age 
)
Y <- data.frame(Y1=3*X$a+
                  X$b+
                  (X$i-50)+
                  rnorm(N,0,1)+
                  (V-5)/NPATIENTS,
                Y2=X$e+
                  X$f+
                  rnorm(N,0,1),
                Y3=X$h+
                  2*X$c+
                  rnorm(N,0,1)+
                  (V-5)/NPATIENTS*2,
                Y4=X$a*X$h+
                  rnorm(N,0,1)
)

cor(Y)
apply(X, 2, function(z) ar(z)$order)
apply(Y, 2, function(z) ar(z)$order)

################################################################################

require(rstan)
require(parallel)
require(lme4)

set_cppo("fast")
#set_cppo("debug")

dat <- list(N=nrow(X),
            X=as.matrix(cbind(rep(1,nrow(X)), X)),
            xa=X$a,
            xb=X$b,
            xc=X$c,
            xd=X$d,
            xe=X$e,
            xf=X$f,
            xg=X$g,
            xh=X$h,
            xi=X$i,
            Y1=Y$Y1,
            Y2=Y$Y2,
            Y3=Y$Y3,
            Y4=Y$Y4,
            Y=as.matrix(Y),
            V=V,
            NP=NPATIENTS)

model.matrix(dat$Y ~ as.factor(V) -1))


################################################################################
# Univariate responses

fitting_code <- '
data {
  int<lower=0> N;
  vector[N] xa;
  vector[N] xb;
  vector[N] xc;
  vector[N] xd;
  vector[N] xe;
  vector[N] xf;
  vector[N] xg;
  vector[N] xh;
  vector[N] xi;
  vector[N] Y1;
  vector[N] Y2;
  vector[N] Y3;
  vector[N] Y4;
  vector[N] V;
  int<lower=0> NP;
}
parameters {
  vector[10] beta;
  #real randbeta;
  real<lower=0> sigma;
}
transformed parameters {
}
model {
  Y1 ~ normal(beta[1] + beta[2]*xa + beta[3]*xb+ beta[4]*xc + beta[5]*xd
              + beta[6]*xe + beta[7]*xf + beta[8]*xg + beta[9]*xh 
              + beta[10]*xi, sigma);
}
'
lmfit <- lm(dat$Y1~xa+xb+xc+xd+xe+xf+xg+xh+xi, data=dat)
lmerfit <- lmer(dat$Y1~xa+xb+xc+xd+xe+xf+xg+xh+xi + (1|V), data=dat)

fitting_code_c <- stanc(model_code=fitting_code, verbose=TRUE)
fitting_code_s <- stan_model(stanc_ret=fitting_code_c, verbose=TRUE)

cl <- makeCluster(2)
stopifnot(all(unlist(clusterEvalQ(cl, {require(rstan)}))))
clusterExport(cl, c("fitting_code_s", "dat", "lmfit"))
f <- function(i, object, data, lmfit) 
{
  sampling(object=object, data=data, iter=100, chains=1, chain_id=i, 
           pars=c("beta","sigma"), warmup=20,
           init=list(
             list(beta=coef(lmfit), sigma=var(resid(lmfit)))
           )
  )
}
system.time({
  fits <- parLapply(cl, 1:2, f, object=fitting_code_s, data=dat, lmfit=lmfit)
})
fit <- sflist2stanfit(fits)

stopCluster(cl)

print(fit)
plot(fit)
traceplot(fit, pars=c("beta"), nrow=5, ncol=2)

################################################################################

fitting_code <- '
data {
int<lower=0> N;
matrix[N,10] X;
vector[4] Y[N];
vector[N] V;
int<lower=0> NP;
}
parameters {
  matrix[10,4] beta;
  #real randbeta;
  cov_matrix[4] sigma;
}
transformed parameters {
  matrix[N,4] mu;
  mu <- X*beta;
}
model {
  for (i in 1:N) 
  {
    Y[i] ~ multi_normal(to_vector(row(mu, i)), sigma);
  }
}
'

fitting_code_c <- stanc(model_code=fitting_code, verbose=TRUE)
fitting_code_s <- stan_model(stanc_ret=fitting_code_c, verbose=TRUE)
fit <- sampling(fitting_code_s, data=dat, iter=100, chains=2,
                pars=c("beta","sigma"))

print(fit)
plot(fit)

traceplot(fit, pars=c("beta"), nrow=5, ncol=2)

################################################################################
# improve speed

fitting_code <- '
data {
  int<lower=0> N;
  matrix[N,10] X;
  vector[4] Y[N];
  vector[N] V;
  int<lower=0> NP;
}
parameters {
  matrix[10,4] beta;
  #real randbeta;
  cov_matrix[4] sigma;
}
transformed parameters {
  vector[4] mu[N];
  matrix[N,4] mumatrix;
  mumatrix <- X*beta;
  for (i in 1:4)
  {
    for (j in 1:N)
    {
      mu[j][i] <- mumatrix[j,i];
    }
  }
}
model {
  for (i in 1:N)  
  {
    Y[i] ~ multi_normal(mu[i], sigma);
  }
}
'

fitting_code_c <- stanc(model_code=fitting_code, verbose=TRUE)
fitting_code_s <- stan_model(stanc_ret=fitting_code_c, verbose=TRUE)

lmfit <- lm(as.matrix(Y)~as.matrix(X))

fit <- sampling(fitting_code_s, data=dat, iter=100, chains=2,
                pars=c("beta","sigma"),
                init=list(
                  list(beta=coef(lmfit), sigma=cov(resid(lmfit))),
                  list(beta=coef(lmfit)*0.9, sigma=1.1*cov(resid(lmfit)))
                )
)

print(fit)
plot(fit)

traceplot(fit, pars=c("beta"), nrow=5, ncol=2)

################################################################################
## improve speed

fitting_code <- '
data {
int<lower=0> N;
matrix[N,10] X;
vector[4] Y[N];
vector[N] V;
int<lower=0> NP;
}
parameters {
matrix[10,4] beta;
#real randbeta;
matrix[4,4] prec;
}
transformed parameters {
matrix[N,4] mu;
mu <- X*beta;
}
model {
for (i in 1:N) 
  {
    Y[i] ~ multi_normal_prec(to_vector(row(mu, i)), prec);
  }
}
'

fitting_code_c <- stanc(model_code=fitting_code, verbose=TRUE)
fitting_code_s <- stan_model(stanc_ret=fitting_code_c, verbose=TRUE)
fit <- sampling(fitting_code_s, data=dat, iter=100, chains=2,
                pars=c("beta","prec"),
                init=list(
                          list(beta=coef(lmfit), prec=solve(cov(resid(lmfit)))),
                          list(beta=coef(lmfit)*0.9, prec=solve(1.1*cov(resid(lmfit))))
))

print(fit)
plot(fit)

traceplot(fit, pars=c("beta"), nrow=5, ncol=2)
traceplot(fit, pars=c("prec"), nrow=5, ncol=2)
