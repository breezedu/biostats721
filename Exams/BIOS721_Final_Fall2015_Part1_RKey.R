# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721| Final Exam Fall 2015 Solutoin Key ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Part 1

# Question 1 ------------------------------------------------

# Part (a) 
setwd('')     # <-- Set to location of data files on your machine!

WEIGHT <- read.csv('weight.csv',
                   header=FALSE,
                   skip=7,
                   nrow=15,
                   stringsAsFactors=FALSE)
w.names <- read.csv('weight.csv',
                    header=FALSE,
                    skip=23,
                    nrow=1,
                    stringsAsFactors=FALSE)
colnames(WEIGHT) <- w.names

WEIGHT

# Part (b)
library(reshape)
WEIGHT2 <- cast(WEIGHT,subject + age ~ time,value='weight')

# Part (c)
temp <- apply(WEIGHT2[,3:5],1,mean,na.rm=T)
WEIGHT2$AVGW <- temp

# Part (d) 
temp <- apply(WEIGHT2[,3:5],1,max)
WEIGHT2$MAXW <- temp

# Part (e)
x <- 0:3
plot(x,rep(NA,length(x)),
     ylim=c(100,200),
     main='Baseline vs. End of Study Weights by Participant',
     ylab='Weight (lbs)',
     xlab='Study Time (months)')

for (i in 1:dim(WEIGHT2)[1]) { 
  lines(c(x[1],x[length(x)]),WEIGHT2[i,c(3,5)],
        type='b',pch=19,col='blue',
        cex=1.5,lwd=2)}

# Part (f)
x <- 0:3
plot(x,rep(NA,length(x)),
     ylim=c(100,200),
     main='Baseline vs. End of Study Weights by Participant',
     ylab='Weight (lbs)',
     xlab='Study Time (months)')

for (i in 1:dim(WEIGHT2)[1]) { 
  if (WEIGHT2$age[i]>=40) {lines(c(x[1],x[length(x)]),WEIGHT2[i,c(3,5)],
               type='b',pch=19,col='red',
               cex=1.5,lwd=2)
    } else {lines(c(x[1],x[length(x)]),WEIGHT2[i,c(3,5)],
        type='b',pch=19,col='blue',
        cex=1.5,lwd=2)}}

legend(2,200,c('Age < 40','Age 40+'),
       col=c('blue','red'),lty=c(1,1))

# Question 2 ------------------------------------------------

A <- 1:3

# Part (a)
A-1

# Part (b)
A*A

# Part (c)
matrix(A,2,3)

# Part (d)
diag(A)

# Question 3 ------------------------------------------------

# Part (a)
toupper(letters)

# Part (b)
set.seed(123)
sample(letters,1)

# Part (c)
set.seed(123)
count <- 0
num.w <- 0 
while(num.w<10) {
  count <- count+1
  if(sample(letters,1)=='w'){num.w <- num.w+1}}
count

# Question 4 ------------------------------------------------

m <- matrix(c(1,1,3,5,2,6,-2,-1,-3),3,3,byrow=T)

Double.Odd <- function(m) {
  m[m%%2==1] <- 2*m[m%%2==1]
  return(list(All.Even=m))}

Double.Odd(m)

# Question 5 ------------------------------------------------

# Part (a)
set.seed(111)
no.runs <- 10000
family <- rep(0,no.runs)
for (i in 1:no.runs){
  child1 <- rbinom(1,1,0.5)
  child2 <- rbinom(1,1,0.5)
  child3 <- rbinom(1,1,0.5)
  if(child1==1&child2==1&child3==1) { family[i] <- 1 }}
mean(family)
  
#> mean(family)
#[1] 0.1235
#> 1/8
#[1] 0.125

# Part (b)
set.seed(222)
no.runs <- 10000
family <- rep(0,no.runs)
for (i in 1:no.runs){
  child1 <- rbinom(1,1,0.5)
  child2 <- rbinom(1,1,0.5)
  child3 <- rbinom(1,1,0.5)
  if((child1==1&child2==1)|(child2==1&child3==1)) { family[i] <- 1 }}
mean(family)

#> mean(family)
#[1] 0.3677
#> 3/8
#[1] 0.375

# -----------------------------------------------------------
# End of Program 
