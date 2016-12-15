##
## 2014 finals
##

# set work directory
setwd("D:/GitHub/biostats721/Exams")


drugA <- c(7,5,8,8)
drugB <- c(6,8,11,12)
week <- c(0,2,4,6)

plot(week,drugA,type='l',col=2,lwd=2,
     main='CD4 Count Trajectories',
     ylab='Average',
     xlab='Weeks sinces enrollment',
     ylim=c(0,15))
points(week,drugB,type='b',col=4,lwd=2)
legend(0,15,
       c('Drug A','Drug B'),col=c(2,4),lty=1)



################
## q2

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


set.seed(123)
Trap_Days(20, c(0.25, 0.25, 0.25, 0.25))$days


## part b
num <- 20
runs <- 1000
array.days <- rep(NA, runs)
prob <- rep(0.25, 4)

set.seed(222)
for( i in 1:runs){
  
  array.days[i] <- Trap_Days(num, prob)$days
}

mean(array.days)
sd(array.days)


## part c

sim.days <- function(N, runs, prob, seeds){
  
  print(N)
  print(runs)
  print(prob)
  print(seeds)
  
  array.days <- rep(NA, runs)
  
  set.seed(seeds)
  
  for( i in 1:runs){
    
    array.days[i] <- Trap_Days(N, prob)$days
  }
  
  u <-  mean(array.days)
  sd <- sd(array.days)
  
  print(u)
  print(sd)
  #print(array.days)
  return( list(mean.days = u, sd.days =sd) )
}

sim.days(20, 1000, rep(0.25, 4), 222)


## part d

sim1 <- sim.days(20, 1000, rep(1/4, 4), 222)
sim2 <- sim.days(20, 1000, rep(1/5, 5), 333)
sim3 <- sim.days(20, 1000, rep(1/6, 6), 444)
sim4 <- sim.days(20, 1000, rep(1/7, 7), 444)
sim5 <- sim.days(20, 1000, rep(1/8, 8), 444)

data.table <- rbind(sim1, sim2, sim3, sim4, sim5)
data.table

row.names <- NULL

for( i in 4:8){
  
  row.names <- c(row.names, paste("No.Species=", i, sep = ''))
  
}
row.names
rownames(data.table) <- row.names
data.table


##########################
## part a
data1 <- read.csv(file = 'data1.csv', nrows = 5, header = T, stringsAsFactors = F)
data1

data2 <- read.csv(file = 'data2.csv', skip = 2, nrows = 3, header = T, stringsAsFactors = F)
data2

## part b
colnames(data2)[1] <- 'Site_ID'
data2

data <- rbind(data1, data2)
data

data$Hosp
data$Enroll[1]

rows <- dim(data)[1]
E.Date <- rep(NA, rows)
E.Date

for( i in 1:rows){
  
  print(data$Hosp[i])
  
  if(data$Hosp[i] == 'acd'){
    
    E.Date[i] <- as.Date(data$Enroll[i], format = '%m/%d/%Y')
    
  } else {
    
    E.Date[i] <- as.Date(data$Enroll[i], format = '%m-%d-%Y')
    
  }
  
}

rows
E.Date
E.Date <- as.Date(E.Date, origin = '1970-1-1')

E.Date
###########
mp <- 25
my.rod <- c(mp)

while(length(my.rod) < 10 & mp<200){
  print(my.rod)
  mp <- mp*1.5
  my.rod <- c(my.rod, mp)
  ## print(my.rod)
}

mp
length(my.rod)

##########################################################
## 2015

weight <- read.csv(file = 'weight.csv', skip = 6, nrows = 15, header = T, stringsAsFactors = F )
weight

weight.names <- read.csv(file = 'weight.csv', skip = 23, nrows = 1, header = F, stringsAsFactors = F)
weight.names

colnames(weight) <- weight.names
weight 

library(reshape)
cast()
weight2 <- cast(weight, subject + age ~ time, value = 'weight')
weight2 <- cast(weight, age ~ time, value = 'weight')
weight2

subject <- cast(weight, subject+age ~ time, value = 'weight')
subject

weight2 <- subject
weight2
average.weight <- apply(weight2[, 3:5], 1, mean, na = T)
av.weight <- apply(weight2[, 3:5], 1, mean, na = T )
av.weight

weight2$AVGE <- av.weight
weight2

max.weight <- apply(weight2[, 3:5], 1, max, na = F)
max.weight

weight2$MAX <- max.weight
weight2

x <- 0:3
plot(x,rep(NA,length(x)),
     ylim=c(100,200),
     main='Baseline vs. End of Study Weights by Participant',
     ylab='Weight (lbs)',
     xlab='Study Time (months)')

for( i in 1:dim(weight2)[1]){
 
  lines(c(x[1],x[2], x[4]), c(weight2[i, 3:5]), type = 'b', col='blue' ) 
  
  
}



x <- 0:3
plot(x,rep(NA,length(x)),
     ylim=c(100,200),
     main='Baseline vs. End of Study Weights by Participant',
     ylab='Weight (lbs)',
     xlab='Study Time (months)')


## part 4
weight2[1,]

for( i in 1:dim(weight2)[1]){
  
  if(weight2[i, 2] > 40){
    color <- 'red'
  } else {
    color <- 'blue'
  }
    
  lines(c(x[1],x[2], x[4]), c(weight2[i, 3:5]), type = 'b', col=color, pch=19, cex=1.5, lwd = 3 ) 
}
legend(2, 200, c('age>40', 'age<40'), col = c('blue', 'red'), lty = c(1,1))

length(c(weight2[1, 3:5]))
length(c(x[1],x[length(x)]))
c(x[1],x[length(x)])


letters
toupper(letters)
