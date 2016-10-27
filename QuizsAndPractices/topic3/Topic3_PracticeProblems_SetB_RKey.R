  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Practice Problems: Topic 3 Solution Key ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set B | Open-Technology Practice Problems

# ----------------------------------------------------

# Question 1: 

X <- c(7, 12, 9, 15, NA, 8, 14, NA, 2, 9, NA, 8)

temp <- NULL
for(item in X){
  
  if(!is.na(item)) temp <- c(temp, item)
}
temp

# Part (a)
new.X <- c()
for (i in 1:12) {
  if (!is.na(X[i])) {new.X <- c(new.X,X[i])} }
new.X

which(!is.na(X))
index <- which(!is.na(X))
temp.x2 <- X[index]
temp.x2

# Part (b)
who <- which(!is.na(X))
new.X2 <- x[who]
new.X2

# Part (c)
new.X <- c()
for (i in 1:length(X)) {
  if (!is.na(X[i])) {new.X <- c(new.X,X[i])} }
new.X

# Part (d)
X <- c(NA,-1,0.5,NA,NA,6)

new.x <- NULL
len <- length(X)

for( item in X){
  if(!is.na(item)) new.x <- c(new.x, item)
}
new.x

new.X <- c()
for (i in 1:length(X)) {
  if (!is.na(X[i])) {new.X <- c(new.X,X[i])} }
new.X

# ----------------------------------------------------

# Question 2: 

# Part (a)
xValue <- 3

while(xValue > 0){
  print(xValue)
  xValue <- xValue - 0.5 }

# Part (b)
# - If you didn't part (a), add a counter to the loop
xValue <- 10
count <- 0

while (xValue > 0) {
  count <- count + 1
  xValue <- xValue - 0.5
  print(xValue)
  
}

count

xValue <- 10
count <- 0

while(xValue > 0){
  count <- count+1
  print(xValue)
  xValue <- xValue - 0.5 }
count

# Part (c)
xValue <- 3
myXs <- c()

while(xValue > 0 & length(myXs) < 8){
  myXs <- c(myXs,xValue)
  print(myXs)
  xValue <- xValue - 0.5 }

xValue <- 10
myXs <- NULL
count <- 0

while( xValue > 0 & length(myXs) < 8){
  
  myXs <- c(myXs, xValue)
  xValue <- xValue - 0.5
  
  print( myXs )
  
  count <- count + 1
}
count

myXs

# Part (d)
# - If you didn't part (c), add a counter to the loop
xValue <- 10 
myXs <- c()
count <- 0

while(xValue > 0 & length(myXs) < 8){
  count <- count+1
  myXs <- c(myXs,xValue)
  print(myXs)
  xValue <- xValue - 0.5 }
count

# ----------------------------------------------------

# Question 3: 

gender <- c('Male','Male','Female',NA,'Female',NA,'Male','Female')
hsCRP <- c(8.00,NA,10.11,7.85,5.04,9.32,6.42,8.05)
data <- data.frame(gender,hsCRP)
data

# Part (a)
risk.CRP <- ifelse(hsCRP>9,'High Risk','Low Risk')
data$risk.CRP <- risk.CRP
data
table(data$risk.CRP,useNA='ifany')
#############################
risk <- ifelse(data$hsCRP > 9, 'highrisk', 'lowrisk')
data$risk <- risk

data

table(data$risk, useNA = 'ifany')

# Part (b)
risk.CRPgender <- ifelse(hsCRP>9,'High Risk',
                    ifelse((gender=='Male' & hsCRP>7.1),'High Risk',
                      ifelse((gender=='Female' & hsCRP>7.1),'Moderate Risk',
                             'Low Risk')))
data$risk.CRPgender <- risk.CRPgender
data
# - Univariate table based on new criteria
table(data$risk.CRPgender,useNA='ifany') 
# - Crosstab comparing old vs. new criteria 
table(data$risk.CRP,data$risk.CRPgender,useNA='ifany')

# ----------------------------------------------------

# Question 4:

x <- c(7, 12, 9, 15, NA, 8, 14, NA, 2, 9,1,0)
y <- matrix(x,3,4)

# Part (a)
# - Note: Need to use length(x) instead of 12 to make it generalizable! 
out1<- rep(NA,length(x)-1)
for (i in 1:(length(x)-1)){ out1[i] <- x[i+1]-x[i]  }
out1

# Part (b)
out2 <- x[2:length(x)]-x[1:(length(x)-1)]
out2

# Part (c)
# - Note: Need to use dim(x) instead of 3 and 4 to make it generalizable! 
out3 <- matrix(NA,dim(y)[1],dim(y)[2]-1)
for (i in 1:dim(y)[1]){
  for (j in 1:(dim(y)[2]-1)) {
    out3[i,j] <- y[i,j+1]-y[i,j] }}
out3

###
row <- dim(y)[1]
col <- dim(y)[2]-1
out.new <- matrix(NA, row, col)

for(i in 1:row){
  for( j in 1:col){
    
    out.new[i,j] <- y[i,j+1] - y[i,j]
  }
}
out.new

# Part (d)
out4 <- y[,2:dim(y)[2]]-y[,1:(dim(y)[2]-1)]
out4

# Part (e)
# - To test, just re-define x and re-run code from part (a) and (b)
x <- c(NA,2,3,5,8,12,17,NA)

# - Retry part(a) code
out1<- rep(NA,length(x)-1)
for (i in 1:(length(x)-1)){ out1[i] <- x[i+1]-x[i]  }
out1

# - Retry part(b) code
out2 <- x[2:length(x)]-x[1:(length(x)-1)]
out2

# Part (f)
# - To test, just re-define y and re-run code from part (c) and (d)
y <- matrix(c(1,2,2,3,3,5),2,3,byrow=T)

# - Retry part(c) code
out3 <- matrix(NA,dim(y)[1],dim(y)[2]-1)
for (i in 1:dim(y)[1]){
  for (j in 1:(dim(y)[2]-1)) {
    out3[i,j] <- y[i,j+1]-y[i,j] }}
out3

# - Retry part(d) code
out4 <- y[,2:dim(y)[2]]-y[,1:(dim(y)[2]-1)]
out4

# ----------------------------------------------------

# Question 5:

# Part (a)
N0 <- 2
f <- 0.5 
days <- 0
cells <- 0

while (cells < 180){
  days <- days+1
  cells <- N0*2^(days*f) }
days

# Part (b)
N0 <- 2
f <- 0.5 
days <- 0
cells <- 0

growth <- NULL

while (cells < 180){
  days <- days+1
  cells <- N0*2^(days*f)
  growth <- rbind(growth,c(days,cells))}

colnames(growth) <- c('Days','No of Cells')
growth



days.vec <- NULL
cells.vec <- NULL

while (cells < 180){
  
  days <- days + 1
  cells <- N0 * 2^(days*f)
  
  days.vec <- c(days.vec, days)
  cells.vec <- c(cells.vec, cells)
  
}

output <- data.frame(days.vec, cells.vec)

# ----------------------------------------------------

# Question 6:

setwd('')   # <-- Set to location of file on your machine!
psych <- read.csv('psych_scores.csv')

pat.ID <- 'M44M'  # <-- Change for patient ID of interest
results <- psych[psych$ID==pat.ID,-1]  # <-- Get test results 
results <- unlist(results)             # <-- Create a vector with test results 

if (sum(!is.na(results))==0) {
  print('Patient answered ZERO questions')
  print(paste('Check if Patient',pat.ID,'attended last scheduled vist.'))
  
} else if (1<=sum(!is.na(results)) & sum(!is.na(results))<=3) {
  print(paste('Patient',pat.ID,'answered the following questions:'))
  Question <- names(results)[!is.na(results)]
  Score <- results[!is.na(results)]
  out <- data.frame(Question,Score)
  rownames(out) <- c()
  print(out)
  
} else if (4<=sum(!is.na(results)) & sum(!is.na(results))<=7) {
  print(paste('Patient',pat.ID,'answered',sum(!is.na(results)),'questions.'))
  print(paste('The average score across responses was',mean(results,na.rm=TRUE),'.'))
  print(paste('The patient did NOT answer the following questions:'))
  print(names(results)[is.na(results)])
  
} else {
  print(paste('Patient',pat.ID,'answered',sum(!is.na(results)),'questions.'))
  print(paste('The average score across responses was',mean(results,na.rm=TRUE),'.'))
  print(paste('The patient gave a score of 5 for the following questions:'))
  print(names(results)[results==5])
  
}

# ----------------------------------------------------

# Question 7:

setwd('')   # <-- Set to location of file on your machine!
biom <- read.csv('biomarker_levels.csv')

# Part (a)
levels <- as.matrix(biom[,3:22])  
low <- levels<200
high <- levels>1000

cleaned <- c(levels)       # <-- Vectorize data 
cleaned[c(low)] <- 200
cleaned[c(high)] <- 1000
biom2 <- matrix(cleaned,50,20)
colnames(biom2) <- colnames(levels)
biom2 <- data.frame(biom[,1:2],biom2)

# Check that it worked ... 
summary(biom$b1)
summary(biom2$b1)

summary(biom$b17)
summary(biom2$b17)


# Part (b)
setwd('')   # <-- Set to location of file on your machine!
bloqs <- read.csv('biomarker_loqs.csv')

biom3 <- biom
for (i in 3:22) {
  biom3[biom3[,i] < bloqs[(i-2),'Lower.LOQ'],i] <- bloqs[(i-2),'Lower.LOQ']
  biom3[biom3[,i] > bloqs[(i-2),'Upper.LOQ'],i] <- bloqs[(i-2),'Upper.LOQ'] }

# Check that it worked ... 
summary(biom$b1)
bloqs[1,]
summary(biom3$b1)

summary(biom$b17)
bloqs[17,]
summary(biom3$b17)


# Part (c)
lloqs <- matrix(bloqs$Lower.LOQ,50,20,byrow=TRUE)
uloqs <- matrix(bloqs$Upper.LOQ,50,20,byrow=TRUE)

levels <- as.matrix(biom[,3:22])  
low <- levels<lloqs
high <- levels>uloqs

cleaned <- c(levels)       # <-- Vectorize data 
cleaned[c(low)] <- c(lloqs)[c(low)]
cleaned[c(high)] <- c(uloqs)[c(high)]
biom4 <- matrix(cleaned,50,20)
colnames(biom4) <- colnames(levels)
biom4 <- data.frame(biom[,1:2],biom4)

# Check that it worked ... 
summary(biom$b1)
bloqs[1,]
summary(biom4$b1)

summary(biom$b17)
bloqs[17,]
summary(biom4$b17)


# Part (d)

start <- proc.time() 
# Part (b)
biom3 <- biom
for (i in 3:22) {
  biom3[biom3[,i] < bloqs[(i-2),'Lower.LOQ'],i] <- bloqs[(i-2),'Lower.LOQ']
  biom3[biom3[,i] > bloqs[(i-2),'Upper.LOQ'],i] <- bloqs[(i-2),'Upper.LOQ'] }
end <- proc.time()
comp.time.b <- end-start; comp.time.b

start <- proc.time() 
# Part (c)
lloqs <- matrix(bloqs$Lower.LOQ,50,20,byrow=TRUE)
uloqs <- matrix(bloqs$Upper.LOQ,50,20,byrow=TRUE)
levels <- as.matrix(biom[,3:22])  
low <- levels<lloqs
high <- levels>uloqs
cleaned <- c(levels)       # <-- Vectorize data 
cleaned[c(low)] <- c(lloqs)[c(low)]
cleaned[c(high)] <- c(uloqs)[c(high)]
biom4 <- matrix(cleaned,50,20)
colnames(biom4) <- colnames(levels)
biom4 <- data.frame(biom[,1:2],biom4)
end <- proc.time()
comp.time.c <- end-start; comp.time.c


# Part (e)
setwd('')   # <-- Set to location of file on your machine!
mloqs <- read.csv('machine_loqs.csv')

biom5 <- biom
for (i in 3:22) {
  for (j in 1:3) {
    lloq <- mloqs[j,'Lower.LOQ']
    uloq <- mloqs[j,'Upper.LOQ']
    biom5[biom5[,i]<lloq & biom5$Machine==j,i] <- lloq
    biom5[biom5[,i]>uloq & biom5$Machine==j,i] <- uloq   }}

# Check that it worked ... 
mloqs

by(biom$b1,biom$Machine,summary)
by(biom5$b1,biom5$Machine,summary)

by(biom$b17,biom$Machine,summary)
by(biom5$b17,biom5$Machine,summary)


# Part (f)
# - Add LOQ data to biomarker data 
biom <- merge(biom,mloqs,by=c('Machine'))

lloqs <- matrix(biom$Lower.LOQ,50,20)
uloqs <- matrix(biom$Upper.LOQ,50,20)

levels <- as.matrix(biom[,3:22])  
low <- levels<lloqs
high <- levels>uloqs

cleaned <- c(levels)       # <-- Vectorize data 
cleaned[c(low)] <- c(lloqs)[c(low)]
cleaned[c(high)] <- c(uloqs)[c(high)]
biom6 <- matrix(cleaned,50,20)
colnames(biom6) <- colnames(levels)
biom6 <- data.frame(biom[,1:2],biom6)

# Check that it worked ... 
mloqs

by(biom$b1,biom$Machine,summary)
by(biom6$b1,biom6$Machine,summary)

by(biom$b17,biom$Machine,summary)
by(biom6$b17,biom6$Machine,summary)

# ----------------------------------------------------

# Question 8:

setwd('')   # <-- Set to location of file on your machine!
glu <- read.csv('base_glucose.csv')

samp1 <- NULL  # Instantiate data objects to store sample data values
samp2 <- NULL
s1 <- 0        # Instantiate sample sds at 0
s2 <- 0
counter <- 4   # Instantiate counter at 4 (min sample size to est sd is 2)
while(abs(60-s1)>5 | abs(60-s2)>5){
  who <- 1:counter
  samp1 <- glu$base_glu[who[who%%2!=0]]
  samp2 <- glu$base_glu[who[who%%2==0]]
  s1 <- sd(samp1)
  s2 <- sd(samp2)
  counter <- counter+2}

# Examine results of while loop
counter
length(samp1)
length(samp2)
sd(samp1)
sd(samp2)

# Check that while loop worked correctly 
who <- 1:(counter-4)              # One iteration too few
sd(glu$base_glu[who[who%%2!=0]])
sd(glu$base_glu[who[who%%2==0]])

who <- 1:(counter-2)              # Required sample size
sd(glu$base_glu[who[who%%2!=0]])
sd(glu$base_glu[who[who%%2==0]])

who <- 1:counter                  # One iteration too many
sd(glu$base_glu[who[who%%2!=0]])
sd(glu$base_glu[who[who%%2==0]])


# ----------------------------------------------------

# Question 9: 

# Part (a)
set.seed(123)
ID <- 1:99
what <- c(rep('I',66),rep('C',33))
Trt.Assign <- sample(what,99,replace=FALSE)
Scheme1 <- data.frame(ID,Trt.Assign)

# - Check that code works: 
head(Scheme1)
tail(Scheme1)
table(Scheme1$Trt.Assign)

# Part (b)
# - Solution using a looping structure
set.seed(456)
ID <- 1:99
Block <- rep(NA,99)
Trt.Assign <- rep(NA,99)
for (block in 1:33) {
  what <- sample(c('I','I','C'),3,replace=FALSE)
  Trt.Assign[(3*block-2):(3*block)] <- what
  Block[(3*block-2):(3*block)] <- rep(block,3)}
Scheme2 <- data.frame(ID,Block,Trt.Assign)
  
# - Solution using vectorized coding 
set.seed(456)
ID <- 1:99
Block <- c(matrix(1:33,3,33,byrow=TRUE))
what <- matrix(c('I','I','C'),3,33)
Trt.Assign <- c(apply(what,2,sample,replace=FALSE))
Scheme2 <- data.frame(ID,Block,Trt.Assign)

# - Check that code works: 
head(Scheme2)
tail(Scheme2)
table(Scheme2$Trt.Assign)
aggregate(Scheme2$Trt.Assign,
          by=list(Scheme2$Block),
          FUN=table)

# Part (c)
set.seed(789)
Block <- c()
Trt.Assign <- c()
counter <- 0
while(length(Trt.Assign)<99){
  counter <- counter+1
  flip <- rbinom(1,1,0.5)
  if (flip==1){
    Block <- c(Block,rep(counter,3))
    Trt.Assign <- c(Trt.Assign,
                    sample(c('I','I','C'),3))
  } else {
    Block <- c(Block,rep(counter,6))
    Trt.Assign <- c(Trt.Assign,
                    sample(c('I','I','C','I','I','C'),6))
  }}
ID <- 1:length(Trt.Assign)
Scheme3 <- data.frame(ID,Block,Trt.Assign)

# - Check that code works: 
head(Scheme3)
tail(Scheme3)
table(Scheme3$Trt.Assign)
aggregate(Scheme3$Trt.Assign,
          by=list(Scheme3$Block),
          FUN=table)

# ----------------------------------------------------
# End of Program
