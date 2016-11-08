# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Quiz 8 Solution Key ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

INFO <- matrix(c(1,3,NA,4,NA,2,2,2,2,2,0,-5,NA,1,4),6,5)
INFO

STUFF <- matrix(1:20,2,10)
STUFF

# --------------------------------------------------------------
# Question 1
row <- dim(INFO)[1]
col <- dim(INFO)[2]
M <- NULL

for( i in 1:col){
  
  M <- c(M, min(INFO[,i], na.rm = T))
  
}

M

# --------------------------------------------------------------
# Question 2
row <- dim(INFO)[1]
col <- dim(INFO)[2]
M <- NULL
i <- 1

while( i <= col){
  
  M <- c(M, min(INFO[,i], na.rm = T))
  i <- i + 1
}

M

# --------------------------------------------------------------
# Question 3

row <- dim(INFO)[1]
col <- dim(INFO)[2]
M <- NULL
i <- 1

negative <- F

while( i <= col){
  
  M <- c(M, min(INFO[,i], na.rm = T))
  
  if(min(INFO[,i], na.rm = T) < 0) {
    negative <- T
  }
  
  i <- i + 1
}

M

## check if there's any minium value less than Zero.
if(negative) {
  print('Some mins are below 0')
}

# --------------------------------------------------------------
# Question 4: BONUS

GET_MINS <- function(mMatrix){
  row <- dim(mMatrix)[1]
  col <- dim(mMatrix)[2]
  M <- NULL
  i <- 1
  
  while( i <= col){
    
    M <- c(M, min(mMatrix[,i], na.rm = T))
    i <- i + 1
  }
  
  return(list(M=M))
  
}

## print the result
GET_MINS(STUFF)

# --------------------------------------------------------------
# End of Program