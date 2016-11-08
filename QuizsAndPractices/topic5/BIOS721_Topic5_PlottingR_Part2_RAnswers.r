
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721| Topic 5 Part 2: Plotting in R ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# *** This file contains solutions to the lecture examples! *** 

# Code from Slides -------------------------------------------------------------

# Read in example data:
# - Note: Need to change the working directory!
setwd("")
data <- read.table("Vegetation2.txt",header=TRUE)
str(data)

# ------------------------------------------------------------------------------

setwd("M:\\Teaching\\Duke Courses\\BIOS 721\\2_Lectures\\Topic5_PlottingR\\figures")

# Standard Scatter Plot
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", 
     main = "Scatter plot")

# Give a little wiggle room in plotting window
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", 
     main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 20))

# Change point character to red shaded in dot
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 20),
     pch = 16,col='red')

# Change type/color of point character based on another variable
# - Plot by transect where data was collected
# - Nice because this variable (numeric) already exists in data set
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", 
     main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 20),
     pch = data$Transect)

# - What if variable doesn't exist? Need to create it!
# - Plot by whether data collected 1974 or before vs. after 1974
pch.time <- rep(1,dim(data)[1])
pch.time[data$Time > 1974] <- 16

plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 20),
     pch = pch.time)

# - Can do the same thing with color
pch.time <- rep(1,dim(data)[1])
pch.time[data$Time > 1974] <- 16

col.time <- rep('purple',dim(data)[1])
col.time[data$Time > 1974] <- 'orange'

plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch = pch.time,
     col = col.time)

# Anything we should add to this figure? ... legend?
# - Try it out!
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch = pch.time,
     col = col.time)

legend('topright',inset=0.05,cex=0.8,
       legend=c('Time <= 1974','Time > 1974'),
       pch=c(1,16),col=c('purple','orange'))

# Change point size based on a third variable
# - Suppose Transect 1 and 4 were of interest
# - Can make their points larger
# - Try it out!
pch.time <- rep(1,dim(data)[1])
pch.time[data$Time > 1974] <- 16

col.time <- rep('purple',dim(data)[1])
col.time[data$Time > 1974] <- 'orange'

cex.tran <- rep(1,dim(data)[1])
cex.tran[data$Transect==1 | data$Transect==4] <- 2

plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch = pch.time,
     col = col.time,
     cex = cex.tran)

# ------------------------------------------------------------------------------

# Adding statistical information to plots
# - Suppose you wanted to add fitted linear regression line to plot
# - Can use the lm() function to fit the linear regression line
# - Then use the abline() function to plot the fitted line
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 20),
     pch=19)

plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 20),
     pch=19)

fit <- lm(data$R ~ data$BARESOIL)
abline(fit,lty=2,lwd=2,col='red')

# - Suppopse you wanted to add some text to the plot
# - For example, the R^2 value 
summary(fit)

plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch=19)

fit <- lm(data$R ~ data$BARESOIL)

abline(fit,lty=2,lwd=2,col='red')
text(x=35,y=17,labels='R^2 = 0.31')

# - This is a good start, but we could make this better
# - Let's make it bigger, bold font
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch=19)

fit <- lm(data$R ~ data$BARESOIL)

abline(fit,lty=2,lwd=2,col='red')
text(x=35,y=17,labels='R^2 = 0.31',   
     font=2,cex=1.5)

# - The R^2 is kind of ugly. Let's make it look like a 
#   legit mathematical equation using teh expression() function!
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch=19)

fit <- lm(data$R ~ data$BARESOIL)
abline(fit,lty=2,lwd=2,col='red')

text(x=35,y=17,font=2,cex=1.5,
     labels=expression(paste(R^{2},' = 0.31',sep='')))


# Can also change the font, script, and size of other titles
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "",
     xlim = c(0, 45), ylim = c(4, 19),
     pch=19)

fit <- lm(data$R ~ data$BARESOIL)
abline(fit,lty=2,lwd=2,col='red')

text(x=35,y=17,labels=expression(paste(R^{2},' = 0.31',sep='')),   
     font=2,cex=1.5)

title(main='Scatter Plot',
      cex.main=3,
      family='HersheyScript')

# ------------------------------------------------------------------------------

# Can change the axis information as well 
# - That is, you can change what values are shown on the axes 
#   (i.e. where the tick marks are located)
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter Plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch=19)

# - Do this in steps ... 
# - First take off axes
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter Plot",
     xlim = c(0, 45), ylim = c(4, 19),
     pch=19,
     axes=FALSE)

# - Then put them back on with with the info you want
# - Use the axis() function for this
#   - Tell R where to place tick marks (based on original plot)
#   - Tell R labels of tick marks
#   - Tell R direction and length of tick marks
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter Plot",
     xlim = c(0, 45), ylim = c(4, 20),
     pch=19,
     axes=FALSE)

plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter Plot",
     xlim = c(0, 45), ylim = c(4, 20),
     pch=19,
     axes=FALSE)
axis(2, at = c(0,5,15,10,20), tcl = 1)
axis(1, at = c(0,25,45), labels = c("Covered","Partial","Exposed") )

# Add a box around the figure to make it complete!
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter Plot",
     xlim = c(0, 45), ylim = c(4, 20),
     pch=19,
     axes=FALSE)
axis(2, at = seq(4,20,by=2), tcl = 1)
axis(1, at = c(0,22,45), 
     labels = c("Covered","Partial","Exposed") )
box()

# Add vertical lines to discriminate between soil categories
# - Suppose the cutoff between covered and partially covered ground 
#   is 10% and the cutoff between partially covered and exposed
#   ground is 31%. 
plot(x = data$BARESOIL, y = data$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Scatter Plot",
     xlim = c(0, 45), ylim = c(4, 20),
     pch=19,
     axes=FALSE)
axis(2, at = c(0,5,15,10,20), tcl = 1)
axis(1, at = c(0,22,45), labels = c("Covered","Partial","Exposed") )
box()
abline(v=10,lty=5,col='gray',lwd=3)
abline(v=31,lty=5,col='gray',lwd=3)

# ------------------------------------------------------------------------------

# If you are using par(mfrow=c()) or the par(mfcol=c())
# options when plotting multiple figures, you might
# want to give the figure overall titles/labels and a
# single legend. 
# - Recall the sin/cos example from Topic 5 Part 1: 
par(mfcol=c(2,2))             # Fill in plots down columns
x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="b",main="SIN(X)",pch=1,col=1,lty=1)
plot(x,cos(x),type="b",main="COS(X)",pch=2,col=2,lty=2)
plot(x,2*sin(x),type="b",main="2*SIN(X)",pch=3,col=3,lty=3)
plot(x,2*cos(x),type="b",main="2*COS(X)",pch=4,col=4,lty=4)

par(mfcol=c(2,2),    # Create one 2x2 figure with 4 plots
    mar=c(4,4,2,2),  # Controls margins for each plot: default = (5,4,4,2) (shirnk margins to squeeze plots together)
    oma=c(3,3,3,1))  # Controls margins for overall figure: default = (2,2,2,2) (strecth bottom to include overall legend)
x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="b",main="SIN(X)",pch=1,col=1,lty=1)
plot(x,cos(x),type="b",main="COS(X)",pch=2,col=2,lty=2)
plot(x,2*sin(x),type="b",main="2*SIN(X)",pch=3,col=3,lty=3)
plot(x,2*cos(x),type="b",main="2*COS(X)",pch=4,col=4,lty=4)

par(mfcol=c(2,2),    # Create one 2x2 figure with 4 plots
    mar=c(4,4,2,2),  # Controls margins for each plot: default = (5,4,4,2) (shirnk margins to squeeze plots together)
    oma=c(3,3,3,1))  # Controls margins for overall figure: default = (2,2,2,2) (strecth bottom to include overall legend)
x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="b",main="SIN(X)",pch=1,col=1,lty=1)
plot(x,cos(x),type="b",main="COS(X)",pch=2,col=2,lty=2)
plot(x,2*sin(x),type="b",main="2*SIN(X)",pch=3,col=3,lty=3)
plot(x,2*cos(x),type="b",main="2*COS(X)",pch=4,col=4,lty=4)
mtext('Plot of Different Trig Functions',
      side=3,outer=TRUE,line=1,cex=1.2,font=2)
mtext('Function Values',
      side=2,outer=TRUE,line=1)

par(mfcol=c(2,2),    # Create one 2x2 figure with 4 plots
    mar=c(4,4,2,2),  # Controls margins for each plot: default = (5,4,4,2) (shirnk margins to squeeze plots together)
    oma=c(3,3,3,1))  # Controls margins for overall figure: default = (2,2,2,2) (strecth bottom to include overall legend)
x <- seq(-2*pi,pi,by=pi/8)
plot(x,sin(x),type="b",main="SIN(X)",pch=1,col=1,lty=1)
plot(x,cos(x),type="b",main="COS(X)",pch=2,col=2,lty=2)
plot(x,2*sin(x),type="b",main="2*SIN(X)",pch=3,col=3,lty=3)
plot(x,2*cos(x),type="b",main="2*COS(X)",pch=4,col=4,lty=4)
mtext('Plot of Different Trig Functions',
      side=3,outer=TRUE,line=1,cex=1.2,font=2)
mtext('Function Values',
      side=2,outer=TRUE,line=1)
par(usr=c(0,1,0,1),  # Reset the coordinates 
    xpd=NA)          # Allow plotting outside the plot region
legend(-1.4,-0.35,legend=c('sin','cos','2sin','2cos'), 
       pch=1:4,lty=1:4,col=1:4,bty='n',horiz=TRUE,cex=1.5)


