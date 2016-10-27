# ~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Quiz 7 ~
# ~~~~~~~~~~~~~~~~~~~

# --------------------------------------------------------------
# Question 1

INFO <- matrix(c(1,3,NA,4,NA,2,2,2,2,2,0,-5,NA,1,4),5,3)
INFO
MISSING <- NULL

row <- dim(INFO)[1]
col <- dim(INFO)[2]

row
col

for( i in 1:col){
  
  count <- 0
  
  for( j in 1:row){
    
    if( is.na(INFO[j, i] ) )
      count <- count +1 
  }
  
  MISSING <- c(MISSING, count)
}

MISSING

# --------------------------------------------------------------
# Question 2

INFO <- matrix(c(1,3,NA,4,NA,2,2,2,2,2,0,-5,NA,1,4),6,5)
INFO


MISSING <- NULL
row <- dim(INFO)[1]
col <- dim(INFO)[2]

row
col

for( i in 1:col){
  
  count <- 0
  
  for( j in 1:row){
    
    if( is.na(INFO[j, i] ) )
      count <- count +1 
  }
  
  MISSING <- c(MISSING, count)
}

## PRINT MISSING
MISSING

# --------------------------------------------------------------
# Question 3

INFO <- matrix(c(1,3,NA,4,NA,2,2,2,2,2,0,-5,NA,1,4),5,3)
INFO


INFO <- matrix(1:12,2,6)
INFO

MISSING <- NULL
row <- dim(INFO)[1]
col <- dim(INFO)[2]

row
col

for( i in 1:col){
  
  count <- 0
  
  for( j in 1:row){
    
    if( is.na(INFO[j, i] ) )
      count <- count +1 
  }
  
  MISSING <- c(MISSING, count)
}

##length(MISS)
## check if the sum of MISS is zero
if( sum(MISSING) < 1){
  
    print('NO missing values in INFO')
  
  } else {
    
    print(MISSING)
  }


# --------------------------------------------------------------
# End of Program