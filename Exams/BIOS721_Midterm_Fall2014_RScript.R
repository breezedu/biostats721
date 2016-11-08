# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721| Midterm Exam Fall 2014 ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Question 1 ------------------------------------------------


# Question 2 ------------------------------------------------

# Example of randomly generated trap collection:
rmultinom(1,1,prob=c(0.5,0.2,0.2,0.1))


# Question 3 ------------------------------------------------

# Data for question:
set.seed(3214)
Week1 <- rnorm(100,300,5)
Week2 <- rnorm(100,290,5)
Week3 <- c(rnorm(88,250,8),rep(NA,12))
Week4 <- c(rnorm(73,225,8),rep(NA,27))
Gender <- sample(c('Female','Male'),100,replace=TRUE)
DATA <- data.frame(Week1,Week2,Week3,Week4,Gender)


# -----------------------------------------------------------
# End of Program 
