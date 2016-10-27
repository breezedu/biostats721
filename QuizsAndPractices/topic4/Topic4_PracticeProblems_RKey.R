
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Practice Problems: Topic 4 Solution Key ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ----------------------------------------------------

# Question 1: 

# - Develop code modularly
xVec <- 1:3
out <- xVec^(1:length(xVec))
xVec
out

# - Then wrap code in a function once it works
myF1 <- function(xVec){
  out <- xVec^(1:length(xVec))
  return(list(out1=out)) }

myF1(1:3)

# - Test/validate function 
rm(list=ls())     # <-- Clear workspace before testing function!

myF1 <- function(xVec){
  out <- xVec^(1:length(xVec))
  return(list(out1=out)) }

myF1(1:3)        
myF1(seq(-4,4,by=2))  

# ----------------------------------------------------

# Question 2: 

# - Develop code modularly
xVec <- 1:3
out <- (xVec^(1:length(xVec)))/(1:length(xVec))
xVec
out

# - Then wrap code in a function once it works
myF2 <- function(xVec){
  out <- (xVec^(1:length(xVec)))/(1:length(xVec))
  return(list(out2=out)) }

myF2(1:3)


# - Test/validate function 
rm(list=ls())     # <-- Clear workspace before testing function!

myF2 <- function(xVec){
  out <- (xVec^(1:length(xVec)))/(1:length(xVec))
  return(list(out2=out)) }

myF2(1:3)     
myF2(seq(-4,4,by=2))  

# ----------------------------------------------------

# Question 3: 

# - Develop code modularly
x <- 1
n <- 3
seq <- (x^(1:n))/(1:n)
out <- 1+sum(seq)
seq 
out

if (length(x)!=1) {
  out <- 'Input Not Valid: x should be a single number'
} else if (round(n,0)!=n | n <=0 ) {
  out <- 'Input Not Valid: n should be a positive integer'
} else {
  seq <- (x^(1:n))/(1:n)
  out <- 1+sum(seq)
}
out

# - Test new scenarios (listed in PPs)
x <- 1:3
n <- 3
if (length(x)!=1) {
  out <- 'Input Not Valid: x should be a single number'
} else if (round(n,0)!=n | n <=0 ) {
  out <- 'Input Not Valid: n should be a positive integer'
} else {
  seq <- (x^(1:n))/(1:n)
  out <- 1+sum(seq)
}
out

x <- 1
n <- -2
if (length(x)!=1) {
  out <- 'Input Not Valid: x should be a single number'
} else if (round(n,0)!=n | n <=0 ) {
  out <- 'Input Not Valid: n should be a positive integer'
} else {
  seq <- (x^(1:n))/(1:n)
  out <- 1+sum(seq)
}
out

x <- 1
n <- 0.5
if (length(x)!=1) {
  out <- 'Input Not Valid: x should be a single number'
} else if (round(n,0)!=n | n <=0 ) {
  out <- 'Input Not Valid: n should be a positive integer'
} else {
  seq <- (x^(1:n))/(1:n)
  out <- 1+sum(seq)
}
out

# - Then wrap code in a function once it works

myF3 <- function(x,n){
  if (length(x)!=1) {
    out <- 'Input Not Valid: x should be a single number'
  } else if (round(n,0)!=n | n <=0 ) {
    out <- 'Input Not Valid: n should be a positive integer'
  } else {
    seq <- (x^(1:n))/(1:n)
    out <- 1+sum(seq) }
  return(list(out3=out)) }

myF3(1,3)

# - Test/validate function 
rm(list=ls())     # <-- Clear workspace before testing function!

myF3 <- function(x,n){
  if (length(x)!=1) {
    out <- 'Input Not Valid: x should be a single number'
  } else if (round(n,0)!=n | n <=0 ) {
    out <- 'Input Not Valid: n should be a positive integer'
  } else {
    seq <- (x^(1:n))/(1:n)
    out <- 1+sum(seq) }
  return(list(out3=out)) }

myF3(1,3)          # Original Scenario
myF3(1.5,4)        # New Scenarios
myF3(rep(1,5),2)
myF3(2,-3)
myF3(2,0.25)
myF3(1:4,-2)

# ----------------------------------------------------

# Question 4: 

# Part (a)
make.A <- function(n,r) {
  # Initialize storage matrix 
  # - Because we need to index each element in matrix, 
  #   need to create an empty matrix for storage rather
  #   than a NULL object (because can't stack here)
  A <- matrix(NA,n,n)
  for(i in 1:n) {   		# Go through rows
    for (j in 1:n) { 	# Go through columns
      A[i,j] <- r^abs(i-j) }}
  return(list(A=A)) }

# Part (b)
# (i)
make.A(n=3,r=2)

# (ii)
make.A(n=4,r=-0.5)

# ----------------------------------------------------

# Question 5: 

# Part (a)
# - Develop code modularly 
xVec <- 1:10
maSeq <- rep(NA,length(xVec)-2)
for (i in 1:(length(xVec)-2)) {
  maSeq[i] <- mean(xVec[i:(i+2)])
}
maSeq

# - Wrap it in a function and test/validate
rm(list=ls())     # <-- Clear workspace before testing function!

Moving.Avg1 <- function(xVec){
  maSeq <- rep(NA,length(xVec)-2)
  for (i in 1:(length(xVec)-2)) {
    maSeq[i] <- mean(xVec[i:(i+2)]) }
  return(list(maSeq=maSeq)) }

Moving.Avg1(1:10)

# Part (b)
Moving.Avg1(c(1:5,6:1))

# Part (c)
xVec <- 1:10
maLen <- 5
n <- length(xVec)
caliper <- maLen-1
maSeq <- rep(NA,n-caliper)
for (i in 1:(n-caliper)) {
  maSeq[i] <- mean(xVec[i:(i+caliper)])
}
maSeq

# - Wrap it in a function and test/validate
rm(list=ls())     # <-- Clear workspace before testing function!

Moving.Avg2 <- function(xVec,maLen){
  n <- length(xVec)
  caliper <- maLen-1
  maSeq <- rep(NA,n-caliper)
  for (i in 1:(n-caliper)) {
    maSeq[i] <- mean(xVec[i:(i+caliper)]) }
  return(list(maSeq=maSeq)) }

Moving.Avg2(1:10,5)

# Part (d)
Moving.Avg2(c(1:5,6:1),3)
Moving.Avg2(c(1:5,6:1),4)

# ----------------------------------------------------

# Question 6: 
# - Function 
truncateV <- function(x,lower=NULL,upper=NULL) {
  out <- x
  if (is.null(lower)==FALSE) {out[x<lower] <- lower}
  if (is.null(upper)==FALSE) {out[x>upper] <- upper}
  return(list(trunc.x=out)) }

# - Test Cases: 
set.seed(246)
test <- c(round(rnorm(4,4,2),2),
          NA,
          round(rnorm(3,4,2),2))
test

# - Scenario 1: Do not supply either lower or upper
#               threshold for truncation
fit1 <- truncateV(test)
cbind(test,fit1$trunc.x)

# - Scenario 2: Supply lower but not upper 
#               threshold for truncation
fit2 <- truncateV(test,lower=3)
cbind(test,fit2$trunc.x)

# - Scenario 3: Supply upper but not lower
#               threshold for truncation
fit3 <- truncateV(test,upper=6.5)
cbind(test,fit3$trunc.x)

# - Scenario 4: Supply both lower and upper
#               threshold for truncation
fit4 <- truncateV(test,lower=3,upper=6.5)
cbind(test,fit4$trunc.x)


# ----------------------------------------------------

# Question 7: 

# Part (a)
# Set the values of code parameters: 
# - Variable we might want to change under 
#   different scenarios. 
seed <- 123
n.cs <- 20
n.cn <- 20
prev <- 0.05

# Initialize storage vectors: 
# - Want to store each subject's recruitment id
# - Want to store each subject's cs/cn status

# Note: Use a NULL object here because don't know
#       how many subjects will have to be recruited
#       to get to 20 cases and 20 controls; so we
#       keep stacking as while loop runs
id <- NULL 
status <- NULL 

# Set the seed
set.seed(seed)  
# Initialize patient counter (recruitment id)
count <- 0     

# Keep going throuh loop until have enough cases
while(sum(status)<n.cs) { 
  # Increase patient counter by 1 each time through the loop
  count <- count+1    
  # Generate cs/cn status using prevalence
  cscn <- rbinom(1,1,prev)  
  # If patient is a case, "enroll" them
  if(cscn==1) {    
    status <- c(status,cscn)
    id <- c(id,count)
  } else {      
    # If patient is a control, "enroll" only if don't
    #  have too many controls
    if(cscn==0&sum(status==0)<n.cn) {
      status <- c(status,cscn) 
      id <- c(id,count)}
  }
}

# Create data.frame with recruitment id and cs/cn status
data.frame(id,status)  

# Check to make sure code works correctly
# - Count the number of cases and controls
table(status)

# Part (b)
# How patients had to be enrolled?
tail(data.frame(id,status))

# ----------------------------------------------------

# Question 8:

# - Just copy and paste code from Question 1 inside function()
# - The paramaters set above become the function inputs

pros.cs.cn.sample <- function(seed,n.cs,n.cn,prev) {
  
  id <- NULL 
  status <- NULL 
  
  set.seed(seed)
  count <- 0
  while(sum(status)<n.cs) {
    count <- count+1
    cscn <- rbinom(1,1,prev)
    if(cscn==1) { 
      status <- c(status,cscn)
      id <- c(id,count)
    } else {
      if(cscn==0&sum(status==0)<n.cn) {
        status <- c(status,cscn) 
        id <- c(id,count)}
    }
  }
  
  out <- data.frame(id,status)
  return(list(out=out))                      }

# Check that function works properly: 
# - For original scenario ... 
fit <- pros.cs.cn.sample(seed=123,n.cs=20,n.cn=20,prev=0.05)
table(fit$out[,'status'])
tail(fit$out)

# For a new scenario ...
fit <- pros.cs.cn.sample(seed=456,n.cs=30,n.cn=15,prev=0.03)
table(fit$out[,'status'])
tail(fit$out)


# ----------------------------------------------------
# End of Program
