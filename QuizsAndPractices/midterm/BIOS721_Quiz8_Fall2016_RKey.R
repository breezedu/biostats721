# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Quiz 8 Solution Key ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

INFO <- matrix(c(1,3,NA,4,NA,2,2,2,2,2,0,-5,NA,1,4),6,5)
INFO

STUFF <- matrix(1:20,2,10)
STUFF

# --------------------------------------------------------------
# Question 1

M <- rep(NA,ncol(INFO))
for (i in 1:ncol(INFO)) { M[i] <- min(INFO[,i],na.rm=TRUE) }
M

# --------------------------------------------------------------
# Question 2

M <- rep(NA,ncol(INFO))
counter <- 1
while(counter <= 5) { 
  M[counter] <- min(INFO[,counter],na.rm=TRUE) 
  counter <- counter+1 }
M

# --------------------------------------------------------------
# Question 3

M <- rep(NA,ncol(INFO))
for (i in 1:ncol(INFO)) { M[i] <- min(INFO[,i],na.rm=TRUE) }
if (min(M)<0) {print(M); print('Some mins are below 0')
  } else { print(M) }

# --------------------------------------------------------------
# Question 4: BONUS

GET_MINS <- function(mat) {
  M <- rep(NA,ncol(mat))
  for (i in 1:ncol(mat)) { M[i] <- min(mat[,i],na.rm=TRUE) }
  return(list(M=M)) }

GET_MINS(INFO)
GET_MINS(STUFF)

# --------------------------------------------------------------
# End of Program