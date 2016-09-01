# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721| Midterm Exam Fall 2014 Solutoin Key ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Question 1 ------------------------------------------------

THING <- list(n=matrix(c(2,3,5,10,0,7),2,3),
              s=c("aa","bb","cc","dd","ee"),
              b=c(TRUE,FALSE,TRUE,FALSE,FALSE))

THING[[2]][1] <- "ta"; THING[[2]]


# Question 2 ------------------------------------------------

# Example of randomly generated trap collection:
rmultinom(1,1,prob=c(0.5,0.2,0.2,0.1))

# Part (a)

set.seed(3214)           # Set the seed so the results are reproducible
trapped <- matrix(0,4,1) # Create a storage vector to count No. of Species trapped
days <- 0                # Create a counter to estimate No. of Days set traps

while(sum(trapped>=10)!=4) { # Keep setting traps until have 10 of each species 
  days <- days+1
  trapped <- trapped+rmultinom(1,1,prob=c(0.5,0.2,0.2,0.1))
}

trapped         # Print results ...  
days 

# Part (b)

# - Create function Trap_Days using the code developed in part (a)
Trap_Days <- function(N,probs,seed) {
  set.seed(seed)              # Set the seed so the results are reproducible
  no.s <- length(probs)       # Determine the No. of Species under study
  trapped <- matrix(0,no.s,1) # Create a storage vector to count No. of Species trapped
  days <- 0                   # Create a counter to estimate No. of Days set traps
  
  while(sum(trapped>=N)!=no.s) { # Keep setting traps until have N of each species 
    days <- days+1
    trapped <- trapped+rmultinom(1,1,prob=probs)
  }
  
  return(list(days=days,trapped=trapped)) }

# - Show that the function works for scenario in part (a)
est1 <- Trap_Days(10,probs=c(0.5,0.2,0.2,0.1),seed=3214)
est1$days

# - Show that the function works for another scenario 
est2 <- Trap_Days(7,probs=c(0.3,0.2,0.2,0.1,0.15,0.05),seed=555)
est2$days


# Question 3 ------------------------------------------------

# Data for question:
set.seed(3214)
Week1 <- rnorm(100,300,5)
Week2 <- rnorm(100,290,5)
Week3 <- c(rnorm(88,250,8),rep(NA,12))
Week4 <- c(rnorm(73,225,8),rep(NA,27))
Gender <- sample(c('Female','Male'),100,replace=TRUE)
DATA <- data.frame(Week1,Week2,Week3,Week4,Gender)

# Part (a)
MALES <- DATA[which(DATA$Gender=="Male"),]

# Part (b)
# - Compute summary measures by column using the apply() fucntion  
#   with dimension set to 2
Means <- apply(MALES[,1:4],2,mean,na.rm=TRUE)
StdDev <- apply(MALES[,1:4],2,sd,na.rm=TRUE)
StatsM <- rbind(Means,StdDev)
StatsM

# Part (c)
x <- 1:4            # Create vector x-coordinate points (can skip)
plot(x,StatsM[1,],  # Pull off means to plot 
     type='b',      # Plot both points and lines
     col='red',     # Color the points / lines red
     pch=19,        # Select shaded circle for plotting character
     cex=1.5,       # Increase size of plotting character
     lty=2,         # Select dashed line for plotting line
     lwd=2,         # Increase width of plotting line
     main='Biomarker Levels among Males',
     ylab='Mean biomarker level',
     xlab='Week')


# Part (d)
x <- 1:4            # Create vector x-coordinate points (can skip)
plot(x,StatsM[1,],  # Pull off means to plot 
     type='b',      # Plot both points and lines
     col='red',     # Color the points / lines red
     pch=19,        # Select shaded circle for plotting character
     cex=1.5,       # Increase size of plotting character
     lty=2,         # Select dashed line for plotting line
     lwd=2,         # Increase width of plotting line
     ylim=c(210,310), # Set limits of y-xis so no data is cutoff 
     main='Biomarker Levels among Males',
     ylab='Mean biomarker level',
     xlab='Week')
# - Add vertical bars around each plotted mean to represent 1SD
segments(1,StatsM[1,1]-StatsM[2,1],1,StatsM[1,1]+StatsM[2,1])
segments(2,StatsM[1,2]-StatsM[2,2],2,StatsM[1,2]+StatsM[2,2])
segments(3,StatsM[1,3]-StatsM[2,3],3,StatsM[1,3]+StatsM[2,3])
segments(4,StatsM[1,4]-StatsM[2,4],4,StatsM[1,4]+StatsM[2,4])


# -----------------------------------------------------------
# End of Program 
