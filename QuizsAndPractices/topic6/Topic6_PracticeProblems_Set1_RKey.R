
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Practice Problems: Topic 6 Set 1 Solution Key ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Question 1: -----------------------------------------

# Part (a)
set.seed(111); rbinom(1,1,0.5)

# Part (b)
set.seed(222); rbinom(5,10,0.5)

# Part (c)
set.seed(333); rbinom(5,1:5,0.5)

# Part (d)
set.seed(444); rbinom(5,1,seq(0.1,0.9,0.2))

# Part (e)
set.seed(555); rbinom(5,1:5,seq(0.1,0.9,0.2))


# Question 2: -----------------------------------------

# Part (a)
n <- 30 
bdays <- 1:365

set.seed(30)
class.bdays <- sample(bdays,n,replace=TRUE)
no.matches <- sum(duplicated(class.bdays))
no.matches


# Part (b)
prob.bday.match <- function(n,seed,runs) {
  set.seed(seed)
  no.matches <- rep(NA,runs)
  for (i in 1:runs){
    class.bdays <- sample(bdays,n,replace=TRUE)
    no.matches[i] <- sum(duplicated(class.bdays)) }
  prob <- sum(no.matches>0)/runs
  return(prob) 
  }

prob.bday.match(n=30,seed=30,runs=1000)


# Part (c)
1 - prob.bday.match(n=20,seed=20,runs=1000)


# Part (d)
pbirthday(30)  	  # Check part (b) 
1-pbirthday(20)		# Check part (c)


# Part (e)
prob.bday.match(n=30,seed=30,runs=1000*1000)
1 - prob.bday.match(n=20,seed=20,runs=1000*1000)


# Part (f)
start <- Sys.time()
prob.bday.match(n=30,seed=30,runs=1000*1000)
mid <- Sys.time()
1 - prob.bday.match(n=20,seed=20,runs=1000*1000)
end <- Sys.time()

run.time.both <- end-start; run.time.both 
run.time.sim1 <- mid-start; run.time.sim1
run.time.sim2 <- end-mid;   run.time.sim2


# Question 3: -----------------------------------------

# Part (a)
set.seed(123)
s <- rnorm(50,0,5)


# Part (b)
set.seed(123)
s <- rnorm(50,0,5)
out <- t.test(s)$p.value 
out


# Part (c)
set.seed(123)
pvals <- rep(NA,10000)

for (i in 1:10000) {
  s <- rnorm(50,0,5)
  pvals[i] <- t.test(s)$p.value }

summary(pvals)


# Part (d)
hist(pvals,main='P-Values under Null Hypothesis > One Sample T-test')
box()


# Part (e)
# - Code development 
set.seed(456)           # Change seed from part (c)
s1 <- rnorm(35,10,5)    # Note: Choice of mu does NOT matter; just 
s2 <- rnorm(50,10,3)    #       needs to be the same for s1 and s2
t.test(s1,s2)$p.value 

# - Perform simulation study
set.seed(456)
pvals <- rep(NA,10000)

for (i in 1:10000) {
  s1 <- rnorm(35,10,5)
  s2 <- rnorm(50,10,3)
  pvals[i] <- t.test(s1,s2)$p.value }

hist(pvals,main='P-Values under Null Hypothesis > Two Sample T-test')
box()


# Part (f)
# - Code development 
set.seed(789)           # Change seed from part (c) and (e)
s1 <- rbinom(500,1,0.25)
s2 <- rbinom(500,1,0.50)
t <- table(s1,s2)
chisq.test(t,correct=FALSE)$p.value

# - Perform simulation study
set.seed(789)
pvals <- rep(NA,10000)

for (i in 1:10000) {
  s1 <- rbinom(500,1,0.25)
  s2 <- rbinom(500,1,0.50)
  t <- table(s1,s2)
  pvals[i] <- chisq.test(t,correct=FALSE)$p.value }

hist(pvals,main='P-Values under Null Hypothesis > Chi-Square Test of Indp')
box()

# -----------------------------------------------------

# End of Program
###############################
## practice

set.seed(30)
n <- 30
bdays <- 1:365

class.birthdays <- sample(bdays, n, replace = T)

num.matches <- sum( duplicated(class.birthdays))



##################################
prob.bday.match <- function(n, seeds, runs){
  
  set.seed(seeds)
  #n <- 30
  
  #runs <- 1000
  
  bdays <- 1:365
  
  ## create a vector of matches
  num.matche <- rep(0, runs)
  
  for( run in 1:runs){
    
    class.birthdays <- sample(bdays, n, replace = T)
    num.matche[run] <- sum( duplicated( class.birthdays))
  }
  
  proportion <- sum(num.matche > 0) / runs
  
  return(proportion)
  
}

prob.bday.match(30, 30, 1000)


##
1 - prob.bday.match(20, 20, 1000)




prob.bday.match(30, 30, 1000*1000)

1 - prob.bday.match(20, 20, 1000*1000)

#################################################
set.seed(123)
s <- rnorm(50, 0, 5)

s.pvalue <- t.test(s)
s.pvalue <- s.pvalue$p.value
s.pvalue


