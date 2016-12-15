# ~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Quiz 10 ~
# ~~~~~~~~~~~~~~~~~~~~

# --------------------------------------------------------------
# Question 1

# set seed
set.seed(123)

# create dice
dice <- 1:6


## roll twice, get two rolls
Roll1 <- sample( dice, 1, replace = T) 
Roll2 <- sample( dice, 1, replace = T)

## print
Roll1
Roll2

# --------------------------------------------------------------
# Question 2

# set seed
set.seed(123)

# create dice
dice <- 1:6


# runs
runs <- 10000

equal.to.ten <- rep(0, runs)

for( i in 1:runs){
  
  ## roll twice, get two rolls
  Roll1 <- sample( dice, 1, replace = T) 
  Roll2 <- sample( dice, 1, replace = T)
  
  if((Roll1 + Roll2) == 10) {
    
    equal.to.ten[i] <- 1
    
  }
  
}

## calculate probability
prob <- sum(equal.to.ten)/runs


## print prob
prob


# --------------------------------------------------------------
# End of Program