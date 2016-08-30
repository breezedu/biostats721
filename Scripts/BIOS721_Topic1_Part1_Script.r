
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721|Topic 1 Part 1: Objects in R ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# *LECTURE EXAMPLES ***********************

# Slide 32: 'Object Assignment and Printing'
a <- 30               # Create the object 'a'
a                     # Print the value of a

# Slide 33: 'Scalars'
s1 <- 2 			  # Numeric
s2 <- "Hi!"	       # Character
s3 <- TRUE		  # Logical

s1        # Print scalar objects created above
s2
s3  

# Slide 35: 'What is a logical object?'

logical <- FALSE
not.logical <- "FALSE"

logical
not.logical

logical*1
not.logical*1

logical2 <- F         # Be careful when using logical
logical2              # shortcuts (T and F)!

F <- 2
logical2 <- F
logical2 

# Slide 39: 'Vectors'
v1 <- c(1,2,3,4,5)	  # Hard Code
v2 <- 1:5			  # A useful short cut       

v1        # Print vector objects created above
v2

# Slide 41: 'Matrices'
M1 <- matrix(1:6,3,2)
M2 <- matrix(1:6,3,2,byrow=TRUE)
M3 <- matrix(1:6,3,2,byrow=TRUE,
             dimnames=list(c("R1", "R2","R3"),
                           c("C1","C2")))


M1        # Print matrix objects created above
M2
M3

# Slide 50: 'Matrices'
x <- rep(1,5)
y <- rep(2,5)
z <- rep(3,5)

M.col <- cbind(x,y,z)
M.row <- rbind(x,y,z)

M.col     # Print matrix objects created above
M.row

# Slide 54: 'Arrays'
A1 <- array(1:16,dim=c(4,2,2))
A2 <- array(c(rep(1,4),rep(2,4),rep(3,4)),dim=c(2,2,3),
           dimnames=list(1:2,1:2,c("ones","twos","threes")))

A1        # Print array objects created above
A2

# Slide 58: 'Can't mix modes in vectors/matrices/arrays'
num <- 1
chr <- "A"
log <- TRUE

a <- c(num,chr,log)
mode(a)

b <- c(num,log)
mode(b)

# Slide 61: 'Data Frames'
num <- 1:4
chr <- c("M","F","F","M")
log <- c(T,T,F,T)
D1 <- data.frame(num,chr,log)  # compared to ... 
D2 <- cbind(num,chr,log)

D1        # Print data.frame objects created above
D2

# Slide 63: 'Lists'
mat <- matrix(1:4,4,3)
chr <- c("M","F","F","M")
log <- c(T,T,F,T)
L <- list(mat,chr,log)

L         # Print list object created above

# ------------------------------------------
# End Program
