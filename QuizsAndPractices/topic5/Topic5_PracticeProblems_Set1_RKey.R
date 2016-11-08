
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Practice Problems: Topic 5 Set 1 Solution Key ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ----------------------------------------------------------
# Question 1: 

# Read in example data:
# - Make sure to change working directory!
setwd('')
data <- read.table("marks2.dat",header=TRUE)
str(data)
boxplot(data$exam1)

# Part (a)
boxplot(data$exam1,
        main='Exam 1 Scores',
        ylab='Score out of 100 points')

# Part (b)
# - Multiple options that give the same figure 
boxplot(data$exam1,
        main='Exam 1 Scores',
        ylab='Score out of 100 points',
        col=colors()[639])    # Color Number

     # OR

colors()[639]
boxplot(data$exam1,
        main='Exam 1 Scores',
        ylab='Score out of 100 points',
        col="turquoise4")   # Color Name

     # OR 

GetColorHexAndDecimal <- function(color){
     c <- col2rgb(color)
     sprintf("#%02X%02X%02X %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3])}
GetColorHexAndDecimal("turquoise4")
boxplot(data$exam1,
        main='Exam 1 Scores',
        ylab='Score out of 100 points',
        col="#00868B")   # Color Hexadecimal

# Part (c)
boxplot(data$exam1,
        main='Exam 1 Scores',
        xlab='Score out of 100 points',
        col=colors()[639],
        horizontal=TRUE)

# Part (d)
# - Set working directory to where you want the figure 
#   to be exported (could be different than where the 
#   data file was stored)
# - Should export to directory where you stored the 
#   data unless you change it!
# setwd("") <-- ***
boxplot(data$exam1,
        main='Exam 1 Scores',
        xlab='Score out of 100 points',
        col=colors()[639],
        horizontal=TRUE)

# ----------------------------------------------------------
# Question 2:

# Part (a)
# - Multiple options give the same figure 
exam <- c(data$exam1,data$exam2,data$exam3)
group <- c(rep(1,length(data$exam1)),
           rep(2,length(data$exam2)),
           rep(3,length(data$exam3)))
boxplot(exam~group)

     # OR

boxplot(data$exam1,data$exam2,data$exam3)


# Part (b)
boxplot(exam~group,
        main='Exam Scores',
        ylab='Score out of 100 points',
        names=c('Exam 1','Exam 2','Exam 3'))

# Part (c)
boxplot(exam~group,
        main='Exam Scores',
        ylab='Score out of 100 points',
        names=c('Exam 1','Exam 2','Exam 3'),
        range=0)

# Part (d)
boxplot(exam~group,
        main='Exam Scores',
        ylab='Score out of 100 points',
        names=c('Exam 1','Exam 2','Exam 3'),
        range=0,
        col=colors()[637:639])

# Part (e)
boxplot(exam~group,
        main='Exam Scores',
        ylab='Score out of 100 points',
        names=c('Exam 1','Exam 2','Exam 3'),
        range=0,
        col=colors()[637:639])

exam.means <- apply(data[,2:4],2,mean,na.rm=TRUE)

lines(1:3,exam.means,type='p',
      pch='*',cex=5,col='darkorange2')

# Part (f)
boxplot(exam~group,
        main='Exam Scores',
        ylab='Score out of 100 points',
        names=c('Exam 1','Exam 2','Exam 3'),
        range=0,
        col=colors()[637:639])

exam.means <- apply(data[,2:4],2,mean,na.rm=TRUE)

lines(1:3,exam.means,type='p',
      pch='*',cex=5,col='darkorange2')

text(0.9,50,labels='* denotes the sample mean')

# Part (g)
# - Set working directory to where you want the figure 
#   to be exported (could be different than where the 
#   data file was stored)
# - Should export to directory where you stored the 
#   data unless you change it!
# setwd("") <-- ***
png('AllExamsBoxplot.png')
boxplot(exam~group,
        main='Exam Scores',
        ylab='Score out of 100 points',
        names=c('Exam 1','Exam 2','Exam 3'),
        range=0,
        col=colors()[637:639])

exam.means <- apply(data[,2:4],2,mean,na.rm=TRUE)

lines(1:3,exam.means,type='p',
      pch='*',cex=5,col='darkorange2')

text(0.9,50,labels='* denotes the sample mean')
dev.off()

# ----------------------------------------------------------
# Question 3:

norm.plot.data <- function(mu,sigma) {
     x <- seq(-3.5,3.5,by=0.01)*sigma + mu
     y <- dnorm(x,mean=mu,sd=sigma)
     return(list(x=x,y=y))           }

# Part (a)
norm.01 <- norm.plot.data(mu=0,sigma=1)
norm.02 <- norm.plot.data(mu=0,sigma=2)
norm.21 <- norm.plot.data(mu=2,sigma=1)
norm.22 <- norm.plot.data(mu=2,sigma=2)

# Part (b)
# - Set working directory to where you want the figure 
#   to be exported (could be different than where the 
#   data file was stored)
# setwd("") <-- ***
png(file='pdfsin1.png')
plot(norm.01$x,norm.01$y,
     type="l",col='sienna2',lwd=2,
     ylim=range(norm.01$y,norm.02$y,norm.21$y,norm.22$y),
     xlim=range(norm.01$x,norm.02$x,norm.21$x,norm.22$x),
     main='Different Normal Distributions',
     ylab='Density',
     xlab='Random Variable Values')
lines(norm.02$x,norm.02$y,col='sienna3',lwd=2)
lines(norm.21$x,norm.21$y,col='slategray2',lwd=2)
lines(norm.22$x,norm.22$y,col='slategray4',lwd=2)
legend(x=-7,y=0.4,cex=0.8,lty=rep(1,4),
       legend=c('mu=0 / sigma=1','mu=0 / sigma=2','mu=2 / sigma=1','mu=2 / sigma=2'),
       col=c('sienna2','sienna3','slategray2','slategray4'))
dev.off()

# Part (c)
# - Set working directory to where you want the figure 
#   to be exported (could be different than where the 
#   data file was stored)
# setwd("") <-- ***
png(file='pdfs2by2.png')
par(mfrow=c(2,2))
plot(norm.01$x,norm.01$y,
     type="l",col='sienna2',
     ylim=range(norm.01$y,norm.02$y,norm.21$y,norm.22$y),
     xlim=range(norm.01$x,norm.02$x,norm.21$x,norm.22$x),
     main='Normal Dist with Mu=0 and Sigma=1',
     ylab='Density',
     xlab='Random Variable Values')

plot(norm.21$x,norm.21$y,col='sienna3',
     type="l",
     ylim=range(norm.01$y,norm.02$y,norm.21$y,norm.22$y),
     xlim=range(norm.01$x,norm.02$x,norm.21$x,norm.22$x),
     main='Normal Dist with Mu=2 and Sigma=1',
     ylab='Density',
     xlab='Random Variable Values')

plot(norm.02$x,norm.02$y,col='slategray2',
     type="l",
     ylim=range(norm.01$y,norm.02$y,norm.21$y,norm.22$y),
     xlim=range(norm.01$x,norm.02$x,norm.21$x,norm.22$x),
     main='Normal Dist with Mu=0 and Sigma=2',
     ylab='Density',
     xlab='Random Variable Values')

plot(norm.22$x,norm.22$y,col='slategray4',
     type="l",
     ylim=range(norm.01$y,norm.02$y,norm.21$y,norm.22$y),
     xlim=range(norm.01$x,norm.02$x,norm.21$x,norm.22$x),
     main='Normal Dist with Mu=2 and Sigma=2',
     ylab='Density',
     xlab='Random Variable Values')
dev.off()

# ----------------------------------------------------------
# Question 5:

# Increase plotting margins (especially the left side)
# to give extra room for text labels 
# - Default is (bottom,left,top,right) = (5,4,4,2)
par(mar=c(5,5.5,5,5.5))

x <- 1:8
y <- 1:8

plot(x,y,
     col=1:8,
     cex=4,
     pch=16,
     ylim=c(0,9),
     axes=FALSE,
     main='Standard Colors for col= {Numeric and Character Reference}',
     xlab='X-Axis: col=Numeric Ref / Y-Axis: col=Character Ref',
     ylab='')
axis(1, at = 1:8)
axis(2, at = 0:9, 
     labels = c(NA,'black','red','green','blue',
                'cyan','magenta','yellow','gray',NA),
     las=2)
box()

# ----------------------------------------------------------
# End of Program
