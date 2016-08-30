
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721|Topic 1 Part 2: Objects in R ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# *LECTURE EXAMPLES ***********************

# Slide 4: Indexing > Motivating Example 
# - How to grab all male subject on treatment A?
id <- 200:205
sex <- c('M','M','F','M','F')
dz <- c(0,1,0,0,1)
trt <- c('A','B','B','A','A')
data <- data.frame(sex,dz,trt)
data

# Slide 6: Indexing Vectors
v <- c(1,5,4,0,9); v

v[2]            
v[c(1,4)]       
v[1:3] 
v[7] 
v[-3]

# Slide 8: Indexing Matrices 
M <- matrix(1:6,3,2); M

M[2,1]
M[1,2]
M[,1]
M[2:3,]
M[,-1]

# Slide 11: Indexing Matrices using dimension names
M <- matrix(1:4,2,2,byrow=TRUE)
rownames(M) <- c('r1','r2')
colnames(M) <- c('C1','C2'); M

M['r2','C2']
M['r2','c2']
M['r1',]
M[,'C1']

# Slide 14: Indexing Data Frames
id <- 200:205
sex <- c('M','M','F','M','F')
dz <- c(0,1,0,0,1)
trt <- c('A','B','B','A','A')
D <- data.frame(sex,dz,trt); D

D[1,3]
D[2:4,2]
D[,1]

# Slide 17: Indexing Data Frames using column names
id <- 200:205
sex <- c('M','M','F','M','F')
dz <- c(0,1,0,0,1)
trt <- c('A','B','B','A','A')
D <- data.frame(sex,dz,trt); D

D[1,'sex']
D[,'trt']
D$trt
D$dz[c(1,4)]

# Slide 20: Indexing > Motivating Example 
id <- 200:205
sex <- c('M','M','F','M','F')
dz <- c(0,1,0,0,1)
trt <- c('A','B','B','A','A')
data <- data.frame(sex,dz,trt)
data

# Slide 27: Applying operators to vectors
v <- 1:5; v

v+2   
v*v 	
v%*%v	
v>3 
v!=2 

# Slide 32: Applying operators to matrices
M1 <- matrix(1:4,2,2); M1
M2 <- diag(rep(1,2));  M2

M1/2
M1-M2
M1==3

# Slide 35: Indexing > Motivating Example 
id <- 200:205
sex <- c('M','M','F','M','F')
dz <- c(0,1,0,0,1)
trt <- c('A','B','B','A','A')
data <- data.frame(sex,dz,trt)
data

# Slide 42: Applying R functions 
v <- 1:5; v
sqrt(v)

m <- matrix(c(1,4,9,25),2,2); m
sqrt(m)

# Slide 44: Applying R functions 
v <- 1:5; v
min(v)

m <- matrix(c(1,4,9,25),2,2); m
min(m)

# Slide 46: Applying R functions 
v <- 1:5; v
dim(v)

m <- matrix(c(1,4,9,25),2,2); m
dim(m)

# Slide 49: Other Useful Functions
v <- -3:3; v

sample(v,4)  
sample(v,7)
sample(v,7,replace=TRUE)

# Slide 51: Indexing with Operators & Functions
v <- c(1,5,4,0,9); v

which(v==4)            # Determine which elements of a 
which(v<=4)            # vector satisfy a condition 
which(v>11)

id <- 20:24; id

id[which(v==4)]        # Results of which()to index a vector
id[which(v<=4)]
id[which(v>11)]

# Slide 53: Indexing > Motivating Example 
id <- 200:205
sex <- c('M','M','F','M','F')
dz <- c(0,1,0,0,1)
trt <- c('A','B','B','A','A')
data <- data.frame(sex,dz,trt)
data

# ------------------------------------------
# End Program