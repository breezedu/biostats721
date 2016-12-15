
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Practice Problems: Topic 6 Set 2 Solution Key ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Question 1: -----------------------------------------

# Part (a)
seed <- 1234
n <- 50
sigma <- 20
d <- 5
mu1 <- 110
mu2 <- mu1+d

set.seed(1234)
sample1 <- rnorm(n,mu1,sigma)
sample2 <- rnorm(n,mu2,sigma)

# - Check data generation
summary(sample1)
summary(sample2)


# Part (b)
t.test(sample1,sample2)$p.value


# Part (c)
seed <- 1234
n <- 50
sigma <- 20
d <- 5
mu1 <- 110
mu2 <- mu1+d

no.runs <- 1000
pvals <- rep(NA,no.runs)

set.seed(seed)
for(i in 1:no.runs) {
  sample1 <- rnorm(n,mu1,sigma)
  sample2 <- rnorm(n,mu2,sigma)
  pvals[i] <- t.test(sample1,sample2)$p.value }

power <- mean(pvals<=0.05)
power


# Part (d)
seed <- 1234
n <- 50
mu1 <- 110
mu2 <- mu1+d
sigma <- 20
no.runs <- 1000

diffs <- seq(-20,20,by=2)
no.alts <- length(diffs)
pvals <- matrix(NA, no.alts, no.runs)

set.seed(seed)
for (j in 1:no.alts) {
  mu2 <- mu1+diffs[j]
  for(i in 1:no.runs) {
    sample1 <- rnorm(n,mu1,sigma)
    sample2 <- rnorm(n,mu2,sigma)
    pvals[j,i] <- t.test(sample1,sample2)$p.value }}

power.by.diffs <- apply(pvals<=0.05,1,mean)

# - Examine output
cbind(diffs,power.by.diffs)


# Part (f)
pcurve.2indp.means <- function(seed,n,mu1,sigma,no.runs,diffs) {
  set.seed(seed)
  no.alts <- length(diffs)
  pvals <- matrix(NA,no.alts,no.runs)
  for (j in 1:no.alts) {
    mu2 <- mu1+diffs[j]
    for(i in 1:no.runs) {
      sample1 <- rnorm(n,mu1,sigma)
      sample2 <- rnorm(n,mu2,sigma)
      pvals[j,i] <- t.test(sample1,sample2,var.equal=FALSE)$p.value }}
  
  power.by.diffs <- apply(pvals<=0.05,1,mean)
  return(list(power.curve=power.by.diffs))      }

pcurve.2indp.means(seed=1234,
                   n=50,
                   mu1=110,
                   sigma=20,
                   no.runs=1000,
                   diffs=seq(-20,20,by=2))


# Part (g)
# Note: Can save function from Q6 as a separate script file and then source it
setwd('')
source('pcurve.2indp.means.r')

mu1=110
sigma=20
no.runs=1000
diffs=seq(-20,20,by=2)

# Should pick a new seed for each simulation scenario:
n.10  <- pcurve.2indp.means(seed=7070,n=10, mu1=mu1,
                            sigma=sigma,no.runs=no.runs,
                            diffs=diffs)$power.curve
n.25  <- pcurve.2indp.means(seed=9598,n=25, mu1=mu1,
                            sigma=sigma,no.runs=no.runs,
                            diffs=diffs)$power.curve
n.50  <- pcurve.2indp.means(seed=1234,n=50, mu1=mu1,
                            sigma=sigma,no.runs=no.runs,
                            diffs=diffs)$power.curve
n.100 <- pcurve.2indp.means(seed=5254,n=100,mu1=mu1,
                            sigma=sigma,no.runs=no.runs,
                            diffs=diffs)$power.curve

power.analysis <- cbind(n.10,n.25,n.50,n.100)
rownames(power.analysis) <- paste('Diff =',diffs,by='')
power.analysis

# Question 2: -----------------------------------------

# Part (a)
# - Code development (working towards making a sim function)
# -- One run; one combo
seed <- 123
n <- 50
mu <- 1
set.seed(seed)
s <- rnorm(n,mu,1)
ci <- t.test(s,conf.level=0.95)$conf.int
capture <- (ci[1]<=mu)*(mu<=ci[2])
capture

# -- All runs; one combo
no.runs <- 100
capture <- rep(NA,no.runs)
set.seed(seed)
for (i in 1:no.runs) {
  s <- rnorm(n,mu,1)
  ci <- t.test(s,conf.level=0.95)$conf.int
  capture[i] <- (ci[1]<=mu)*(mu<=ci[2]) }
mean(capture)

# -- All runs; any combo
dist <- 'exp'
n <- c(5,10,15)
capture <- matrix(NA,no.runs,length(n))
set.seed(seed)
seeds <- round(runif(length(n),100,1000),0)
for (j in 1:length(n)) {
  set.seed(seeds[j])
for (i in 1:no.runs) {
  if(dist=='norm') {s <- rnorm(n[j],mu,1)
    } else if (dist=='chisq') {s <- rchisq(n[j],mu)
    } else {s <- rexp(n[j],mu)}
  ci <- t.test(s,conf.level=0.95)$conf.int
  capture[i,j] <- (ci[1]<=mu)*(mu<=ci[2]) }}
colnames(capture) <- paste('n =',n)
apply(capture,2,mean)

# - Simulation function
cp.sim <- function(seed,dist,
                   n=c(5,10,15,20,25,50,100),
                   mu=1,no.runs=10000) {
  
  capture <- matrix(NA,no.runs,length(n))
  
  set.seed(seed)
  seeds <- round(runif(length(n),100,1000),0)
  
  for (j in 1:length(n)) {
    set.seed(seeds[j])
    for (i in 1:no.runs) {
      if(dist=='norm') {s <- rnorm(n[j],mu,1)
      } else if (dist=='chisq') {s <- rchisq(n[j],mu)
      } else {s <- rexp(n[j],mu)}
      ci <- t.test(s,conf.level=0.95)$conf.int
      capture[i,j] <- (ci[1]<=mu)*(mu<=ci[2]) }}
  
  colnames(capture) <- paste('n =',n)
  return(list(capture=capture))}

# - Run simulation: 
fit.norm  <- cp.sim(seed=111,dist='norm')
fit.chisq <- cp.sim(seed=222,dist='chisq')
fit.exp   <- cp.sim(seed=333,dist='exp')


# Part (b)
results <- rbind(apply(fit.norm$capture,2,mean),
                 apply(fit.chisq$capture,2,mean),
                 apply(fit.exp$capture,2,mean))
rownames(results) <- c('N(1,1)','Chisq(df=1)','Exp(rate=1)')
results
round(results,2)


# Part (c)
n <- c(5,10,15,20,25,50,100)

plot(n,rep(NA,length(n)),
     ylim=c(0.8,1.0),
     main='Coverage Proability for One Sample T-test 95% CI',
     ylab='Coverage Probability (based on 10000 runs)',
     xlab='Sample Size')

abline(h=0.95,lty=2,col='black',lwd=1)

lines(n,apply(fit.norm$capture,2,mean),
     type='b',lty=1,col='slateblue2',lwd=2)
lines(n,apply(fit.chisq$capture,2,mean),
      type='b',lty=1,col='turquoise',lwd=2)
lines(n,apply(fit.exp$capture,2,mean),
      type='b',lty=1,col='red',lwd=2)

legend('bottomright',cex=1.25,
       legend=c('N(1,1)','Chisq(df=1)','Exp(rate=1)'),
       col=c('slateblue2','turquoise','red'),
       lty=rep(1,3),lwd=rep(2,3))

# -----------------------------------------------------

# End of Program
