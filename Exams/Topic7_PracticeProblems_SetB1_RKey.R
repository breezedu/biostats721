
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Practice Problems: Topic 7 Set B1 Solution Key ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set B1 | Open-Technology Practice Problems

# Simulation Code: ------------------------------------

# - Simulation function:
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


# Question 1: -----------------------------------------

# - R Code given in problem: 
results <- rbind(apply(fit.norm$capture,2,mean),
                 apply(fit.chisq$capture,2,mean),
                 apply(fit.exp$capture,2,mean))
rownames(results) <- c('N(1;1)','Chisq(df=1)','Exp(rate=1)')
results

# Part (a)
results2 <- round(results,2)
results2

# Part (b)
setwd('')   # <-- Set to desired location 

# - i
write.csv(results2,file='cov_prob_table_v1.csv')
# - ii
write.table(results2,file='cov_prob_table_v1.txt')
# - iii 
write.xlsx(results2,file='cov_prob_table_v1.xlsx')

# Part (c)
results3 <- formatC(results,digits=2,format='f')
results3

setwd('')   # <-- Set to desired location 
# - i 
write.csv(results3,file='cov_prob_table_v2a.csv',quote=TRUE)
write.csv(results3,file='cov_prob_table_v2b.csv',quote=FALSE)
# - ii
write.table(results3,file='cov_prob_table_v2a.txt',quote=TRUE)
write.table(results3,file='cov_prob_table_v2b.txt',quote=FALSE)
# - iii 
write.xlsx(results3,file='cov_prob_table_v2.xlsx')


# Question 2: -----------------------------------------

# - R Code given in problem:
n <- c(5,10,15,20,25,50,100)

plot(n,rep(NA,length(n)),
     ylim=c(0.8,1.0),
     main='Coverage Proability for T-test 95% CI',
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
       legend=c('N(1;1)','Chisq(df=1)','Exp(rate=1)'),
       col=c('slateblue2','turquoise','red'),
       lty=rep(1,3),lwd=rep(2,3))

# Part (a)
setwd('')   # <-- Set to desired location 
pdf('cov_prob_fig.pdf',width=11,height=9)
n <- c(5,10,15,20,25,50,100)

plot(n,rep(NA,length(n)),
     ylim=c(0.8,1.0),
     main='Coverage Proability for T-test 95% CI',
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
       legend=c('N(1;1)','Chisq(df=1)','Exp(rate=1)'),
       col=c('slateblue2','turquoise','red'),
       lty=rep(1,3),lwd=rep(2,3))
dev.off()

# Part (b)
setwd('')   # <-- Set to desired location 
png('cov_prob_fig.png',width=11,height=9,units='in',res=300)
n <- c(5,10,15,20,25,50,100)

plot(n,rep(NA,length(n)),
     ylim=c(0.8,1.0),
     main='Coverage Proability for T-test 95% CI',
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
       legend=c('N(1;1)','Chisq(df=1)','Exp(rate=1)'),
       col=c('slateblue2','turquoise','red'),
       lty=rep(1,3),lwd=rep(2,3))
dev.off()


# Question 3: -----------------------------------------
# - Compile code in the script file 
#   Topic7_PracticeProblems_SetB1_RKey_Q3.Rmd

# -----------------------------------------------------
# End of Program
