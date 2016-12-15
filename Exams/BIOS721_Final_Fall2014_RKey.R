# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721| Final Exam Fall 2014 ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Question 1 ------------------------------------------------

# Part (a)
drugA <- c(7,5,8,8)
drugB <- c(6,8,11,12)
week <- c(0,2,4,6)

plot(week,drugA,type='l',col=2,lwd=2,
     main='CD4 Count Trajectories',
     ylab='Average',
     xlab='Weeks sinces enrollment',
     ylim=c(0,15))
lines(week,drugB,col=4,lwd=2)
legend(0,15,c('Drug A','Drug B'),col=c(2,4),lty=1)

# Part (b)
plot(week,drugA,type='b',col=2,lwd=2,
     main='CD4 Count Trajectories',
     ylab='Average',
     xlab='Weeks sinces enrollment',
     ylim=c(0,15))
lines(week,drugB,col=4,lwd=2,type='b')
legend(0,15,c('Drug A','Drug B'),col=c(2,4),lty=1)

# Question 2 ------------------------------------------------

Trap_Days <- function(N,probs) {
  # N = Number of birds of each species 
  # probs = probability distribution of each species 
  no.s <- length(probs)       
  trapped <- matrix(0,no.s,1) 
  days <- 0                 
  while(sum(trapped>=N)!=no.s) { 
    days <- days+1
    trapped <- trapped+rmultinom(1,1,prob=probs)} 
  return(list(days=days,trapped=trapped)) } 

# Part (a): 
set.seed(111)
Trap_Days(20,rep(1/4,4))$days

# Part (b): 
N <- 20
probs <- rep(1/4,4)
runs <- 1000
days <- rep(NA,runs)

set.seed(222)
for (i in 1:runs){
  days[i] <- Trap_Days(N,probs)$days
}

mean(days)
sd(days)

# Part (c): 
sim.func <- function(N,probs,runs,seed){
  days <- rep(NA,runs)
  
  set.seed(seed)
  for (i in 1:runs){
    days[i] <- Trap_Days(N,probs)$days}
  m <- mean(days)
  s <- sd(days)
  return(list(mean.days=m,sd.days=s))}

sim.func(20,rep(1/4,4),1000,222)

# Part (d): 
table <- rbind(t(sim.func(20,rep(1/4,4),1000,222)),
               t(sim.func(20,rep(1/5,5),1000,333)),
               t(sim.func(20,rep(1/6,6),1000,444)),
               t(sim.func(20,rep(1/7,7),1000,555)),
               t(sim.func(20,rep(1/8,8),1000,666)))

rownames(table) <- paste('No.Species=',4:8,sep='')
table

# Question 3 ------------------------------------------------

# Part (a):
setwd()
data1 <- read.csv('data1.csv',
                  head=TRUE,
                  nrows=5,
                  stringsAsFactors=FALSE)
data1

data2 <- read.csv('data2.csv',
                  head=TRUE,
                  skip=2,
                  nrows=3,
                  stringsAsFactors=FALSE)
data2

# Part (b): 
names(data2)[1] <- 'Site_ID'
data <- rbind(data1,data2)
data

# Part (c): 
data$MinLab <- apply(data[,6:8],1,min,na.rm=TRUE)
data

# Part (d): 
E.date <- rep(NA,8)
for (i in 1:8) {
  if(data$Hosp[i]=='acd') {
    E.date[i] <- as.Date(data$Enroll[i],format='%m/%d/%Y')
  } else {
    E.date[i] <- as.Date(data$Enroll[i],format='%m-%d-%Y')}}
E.date
data$E.date <- as.Date(E.date,origin='1970-1-1')
data

# Question 4 ------------------------------------------------

myP <- 25
myProd <- c()

while(myP < 200 & length(myProd) < 10){
  myProd <- c(myProd,myP)
  print(myProd)
  myP <- myP*1.5 }

# -----------------------------------------------------------
# End of Program 
