# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721| Final Exam Fall 2014 ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Question 1 ------------------------------------------------


# Question 2 ------------------------------------------------

Trap_Days <- function(N,probs) {
  # N = Number of species 
  # probs = probability distribution of each species 
  no.s <- length(probs)       
  trapped <- matrix(0,no.s,1) 
  days <- 0                 
  while(sum(trapped>=N)!=no.s) { 
    days <- days+1
    trapped <- trapped+rmultinom(1,1,prob=probs)} 
  return(list(days=days,trapped=trapped)) } 


# Question 3 ------------------------------------------------


# Question 4 ------------------------------------------------


# -----------------------------------------------------------
# End of Program 
