
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721| Topic 5 Part 1: Plotting in R ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Code from Slides -------------------------------------------------------------

# Read in example data:
# - Change working directory for your computer!
setwd("")
mk2nd <- read.table("marks2.dat",header=TRUE)

# ~~~~~~~~~~~~~~
# ~ HISTOGRAMS ~
# ~~~~~~~~~~~~~~

hist(mk2nd[,"exam1"])       # Default histogram of exam 1 scores from mk2nd data

hist(mk2nd[,"exam1"],       # Change title and x-axis label
     main="Histogram of Exam 1 Scores",
     xlab="Scores of out 100")

hist(mk2nd[,"exam1"],       # Change the number of bins to 10 *** just a suggestion! ***
     breaks=10,
     main="Histogram of Exam 1 Scores",
     xlab="Scores of out 100")
     
hist(mk2nd[,"exam1"],       # Change the break points to c(0,20,25,30,40)
     breaks=c(0,20,25,30,40),
     main="Histogram of Exam 1 Scores",
     xlab="Scores of out 100")
     

hist(mk2nd[,"exam1"],       # Plot density instead of freguency on y-axis
     freq=FALSE,
     main="Histogram of Exam 1 Scores",
     xlab="Scores of out 100",)
     
     
hist(mk2nd[,"exam1"],       # Overlay the estimated density curve
     freq=FALSE,            # - Example of high vs. low level plotting
     main="Histogram of Exam 1 Scores",
     xlab="Scores of out 100")
d <- density(mk2nd[,"exam1"])
lines(d$x,d$y)

hist(mk2nd[,"exam1"],       # Customize the plot:
     freq=FALSE,            # - changes line to red, dashed, and slightly thicker
     main="Histogram of Exam 1 Scores",
     xlab="Scores of out 100")
d <- density(mk2nd[,"exam1"])
lines(d$x,d$y,col=3,lty=3,lwd=4)

hist(mk2nd[,"exam1"],       # Customize the plot:
     freq=FALSE,            # - changes line to blue, dotted & dashed, and thicker
     main="Histogram of Exam 1 Scores",
     xlab="Scores of out 100")
d <- density(mk2nd[,"exam1"])
lines(d$x,d$y,col="blue",lty=4,lwd=4)

        
# ~~~~~~~~~~~~~~~~~~~~~
# ~ GENERIC X-Y PLOTS ~
# ~~~~~~~~~~~~~~~~~~~~~

plot(mk2nd[,"exam1"],mk2nd[,"exam2"],     # Scatterplot
     main="Scatter Plot of Exam 1 and 2 Scores",
     xlab="Exam 1",
     ylab="Exam 2")
     
plot(mk2nd[,"exam1"],mk2nd[,"exam2"],     # Change color and size of point characters
     main="Scatter Plot of Exam 1 and 2 Scores",
     xlab="Exam 1",
     ylab="Exam 2",
     col="magenta",cex=3)

plot(mk2nd[,"exam1"],mk2nd[,"exam2"],     # Change point characters
     main="Scatter Plot of Exam 1 and 2 Scores",
     xlab="Exam 1",
     ylab="Exam 2",
     pch=17,cex=2,col=3)
     



x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="l",                   # Line plot
     main="Sin Function",
     ylab="sin(x)",
     xlab="x values")
     
x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="b",                   # Line and point plot
     main="Sin Function",
     ylab="sin(x)",
     xlab="x values",
     pch="S",col="purple")

x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="b",                   # Plot multiple lines simultaneously
     main="Trig Functions",
     ylab="f(x)",
     xlab="x values",
     pch="S",col=2,lty=1)
lines(x,cos(x),type="b",pch="C",col=3,lty=2)

x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="b",                   # Problems with the limits of the y-axis
     main="Trig Functions",
     ylab="f(x)",
     xlab="x values",
     pch=1,col=1,lty=1)
lines(x,cos(x),type="b",pch=2,col=2,lty=2)
lines(x,2*sin(x),type="b",pch=3,col=3,lty=3)
lines(x,2*cos(x),type="b",pch=4,col=4,lty=4)

x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="b",                   # Changing the limits of the y-axis manually based on range of all y's.
     main="Trig Functions",
     ylab="f(x)",
     xlab="x values",
     ylim=c(-2.5,2.5),
     pch=1,col=1,lty=1)
lines(x,cos(x),type="b",pch=2,col=2,lty=2)
lines(x,2*sin(x),type="b",pch=3,col=3,lty=3)
lines(x,2*cos(x),type="b",pch=4,col=4,lty=4)

x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="b",                   # Changing the limits of the y-axis using range of all y's.
     main="Trig Functions",
     ylab="f(x)",
     xlab="x values",
     ylim=range(sin(x),cos(x),2*sin(x),2*cos(x)),
     pch=1,col=1,lty=1)
lines(x,cos(x),type="b",pch=2,col=2,lty=2)
lines(x,2*sin(x),type="b",pch=3,col=3,lty=3)
lines(x,2*cos(x),type="b",pch=4,col=4,lty=4)

x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="b",                   # Adding a legend to the plot for line type
     main="Trig Functions",
     ylab="f(x)",
     xlab="x values",
     ylim=range(sin(x),cos(x),2*sin(x),2*cos(x)),
     pch=1,col=1,lty=1)
lines(x,cos(x),type="b",pch=2,col=2,lty=2)
lines(x,2*sin(x),type="b",pch=3,col=3,lty=3)
lines(x,2*cos(x),type="b",pch=4,col=4,lty=4)
legend(-3.5,2,legend=c("sin","cos","2*sin","2*cos"),lty=1:4)

x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="b",                   # Adding color and point characters to legend
     main="Trig Functions",
     ylab="f(x)",
     xlab="x values",
     ylim=range(sin(x),cos(x),2*sin(x),2*cos(x)),
     pch=1,col=1,lty=1)
lines(x,cos(x),type="b",pch=2,col=2,lty=2)
lines(x,2*sin(x),type="b",pch=3,col=3,lty=3)
lines(x,2*cos(x),type="b",pch=4,col=4,lty=4)
legend(-3.5,2,legend=c("sin","cos","2*sin","2*cos"),lty=1:4,col=1:4,pch=1:4)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ FIGURES WITH MULTIPLE PLOTS ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

par(mfcol=c(2,2))             # Fill in plots down columns
x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="b",main="SIN(X)",pch=1,col=1,lty=1)
plot(x,cos(x),type="b",main="COS(X)",pch=2,col=2,lty=2)
plot(x,2*sin(x),type="b",main="2*SIN(X)",pch=3,col=3,lty=3)
plot(x,2*cos(x),type="b",main="2*COS(X)",pch=4,col=4,lty=4)

par(mfrow=c(2,2))             # Fill in plots across rows
x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="b",main="SIN(X)",pch=1,col=1,lty=1)
plot(x,cos(x),type="b",main="COS(X)",pch=2,col=2,lty=2)
plot(x,2*sin(x),type="b",main="2*SIN(X)",pch=3,col=3,lty=3)
plot(x,2*cos(x),type="b",main="2*COS(X)",pch=4,col=4,lty=4)

