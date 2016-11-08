# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Fall 2016 Midterm ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# --------------------------------------------------------------
# Question 1


# Test Cases: 
x1 <- 1:5; x1

x2 <- -2:2; x2

x3 <- seq(-100,-10,by=10); x3

x4 <- matrix(c(-5:-1,1:5),2,5,byrow=T); x4

x5 <- diag(-1,3); x5

POS_SUM <- function( xVec ){
  
  ## check if xVec is a vector
  
  if( is.vector(xVec) ){
    
    sum <- 0
    
    for( i in 1:length(xVec)){
      
      if(xVec[i] > 0 ){
        sum <- sum + xVec[i]
      }
      
    }
    
    
    return ( list(out = sum ) )
    
  } else {
    
    sum <- 0
    
    row <- dim(xVec)[1]
    col <- dim(xVec)[2]
    
    for( i in 1:row){
      
      for( j in 1:col){
        
        if( xVec[i, j] > 0 ){
          sum <- sum + xVec[i, j]
        }
        
      }
    }
    
    print( sum)
    
    return( list(out = sum ))
    
  }
  
}

POS_SUM(x1)
POS_SUM(x2)
POS_SUM(x3)
POS_SUM(x4)
POS_SUM(x5)


# --------------------------------------------------------------
# Question 2

# Part (a)
D1 <- read.table('Data1.txt',
                 skip = 2,
                 stringsAsFactors = F,
                 header = T,
                 sep = "+"
                 )

D1

D2 <- read.table('Data2.txt',
                 stringsAsFactors = F,
                 header = T,
                 sep = "+",
                 nrows = 3
)
D2

D3 <- read.table('Data3.txt',
                 stringsAsFactors = F,
                 header = T,
                 sep = "+",
                 ##nrows = 3
)
D3

# Part (b)

library(gtools)

## bind D1 and D2
# mydata <- merge(mydata1, mydata5, by.x=c("country","year"), by.y=c("nations","time"))
## rename D2$AGEYR
D2$AGE <- D2$AGEYR
D2$AGEYR <- NULL

Data.m <- smartbind(D1, D2, fill = T) 

Data.m

## merge Data.m and D3

Data.m2 <- merge(Data.m, D3, by = c("ID", "SITE"))
Data.m2

## assign WORK
WORK <- Data.m2

WORK

# --------------------------------------------------------------
# Question 3


BigUs <- NULL
count <- 0
while(length(BigUs) < 20 ){
  
  random <- runif(1, 0, 1)
  count <- count + 1
  
  if( random > 0.9){
    
    BigUs <- c(BigUs, random)
    
  }
  ##print(count)
}

## print bigus and counts of loops
BigUs
count




## Answer, we have to run 186 loops this time.

# --------------------------------------------------------------
# End of Program