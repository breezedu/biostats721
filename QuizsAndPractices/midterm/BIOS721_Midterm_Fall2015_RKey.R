# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721| Midterm Exam Fall 2015 Solutoin Key ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Question 1 ------------------------------------------------

# Part (a) 
setwd('')     # <-- Set to location of data files on your machine!

DATA1 <- read.csv('affected.csv',
                  stringsAsFactors=FALSE,
                  header=TRUE,
                  skip=4)
DATA2 <- read.csv('family.csv',
                  stringsAsFactors=FALSE,
                  header=TRUE,
                  nrow=14)

# Part (b)
DATA3 <- merge(DATA1,DATA2,by=c('child.id'))

# Part (c)
all <- DATA3$child.id[DATA3$f.type!='Mother' & 
                      DATA3$f.type!='Father']
# - Option 1: 
extended <- all[!duplicated(all)]
extended

# - Option 2: 
extended <- unique(all)
extended

# Question 2 ------------------------------------------------

# Part (a)
xBars1 <- rep(NA,5)
set.seed(123)
for (i in 1:5) { d <- rnorm(50,8,1)
                  xBars1[i] <- mean(d) }
xBars1

# Part (b)
set.seed(123)
d <- matrix(rnorm(50*5,8,1),50,5)
xBars2 <- apply(d,2,mean)
xBars2

# Question 3 ------------------------------------------------

# Part (a)
# - Develop meat of function code
year <- 1800
if (year%%4!=0) { 
  # Not divisible by 4
  out <- paste(year,'is NOT a leap year.')
    } else if (year%%100==0 & year%%400!=0) { 
    # Divisible by 4 and (by 100 and not by 400)
    out <- paste(year,'is NOT a leap year.')
      } else {
      # Divisible by 4 and (not by 100 or by 400)
      out <- paste(year,'is a leap year!')}
out

# - Function code
is.leap.year <- function(year) {
  if (year%%4!=0) { out <- paste(year,'is NOT a leap year.')
    } else if (year%%100==0 & year%%400!=0) { 
      out <- paste(year,'is NOT a leap year.')
      } else {out <- paste(year,'is a leap year!')}
  return(list(out=out)) }

# Part (b): 
is.leap.year(1996)

# -----------------------------------------------------------
# End of Program 