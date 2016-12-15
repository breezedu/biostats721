
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Practice Problems: Topic 5 Set 2 Solution Key ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ----------------------------------------------------------
# Question 1: 

# Part (a)
setwd('')                               # <-- Set for your machine!
fdata <- read.csv("fluoride.csv")       # Read in data file 
fdata$DIFF <- fdata$AFTER-fdata$BEFORE  # Create primary outcome variable 

plot(fdata$AGE,fdata$DIFF, 
     main='Difference in # of decayed, missing or filled teeth during study period',
     ylab='DMFT After - DMFT Before',
     xlab='Patient age (years)')

# Part (b)
pch <- rep('o',69)
pch[fdata$TRT=='SF'] <- '*'
pch[fdata$TRT=='W'] <- '|'

col <- rep('blue',69)
col[fdata$TRT=='SF'] <- 'green'
col[fdata$TRT=='W'] <- 'magenta'

cex <- rep(1.5,69)
cex[fdata$TRT=='SF'] <- 3

plot(fdata$AGE,fdata$DIFF, 
     main='Difference in # of decayed, missing or filled teeth during study period',
     ylab='DMFT After - DMFT Before',
     xlab='Patient age (years)',
     pch=pch,
     col=col,
     cex=cex)

legend('topleft',
       legend=c('APFluor','SFluor','Water'),
       pch=c('o','*','l'),
       col=c('blue','green','magenta'),
       horiz=T,
       pt.cex=c(1.5,2.2,1.5))


# ----------------------------------------------------------
# Question 2: 

# Part (a)
setwd('')                                     # <-- Set for your machine!
cdata <- read.csv("cholesterol.csv")          # Read in data file 
cases <- subset(cdata,cdata$group=='Case')    # Create subsets 
controls <- subset(cdata,cdata$group=='Control')

boxplot(cdata$totserchol~cdata$group,
        main='Cholesterol Levels in Males',
        names=c('2 days Post-MI','Controls'),
        col='lightgray')

segments(0.6,mean(cases$totserchol),
         1.4,mean(cases$totserchol),
         lty=2,lwd=2)
segments(1.6,mean(controls$totserchol),
         2.4,mean(controls$totserchol),
         lty=2,lwd=2)

points(rep(1,length(cases$totserchol)),
       cases$totserchol,
      cex=1.5,col='sienna3',pch=16)

points(rep(2,length(controls$totserchol)),
       controls$totserchol,
       cex=1.5,col='slateblue2',pch=16)


# Part (b)
set.seed(123)
boxplot(cdata$totserchol~cdata$group,
        main='Cholesterol Levels in Males',
        names=c('2 days Post-MI','Controls'),
        col='lightgray')

segments(0.6,mean(cases$totserchol),
         1.4,mean(cases$totserchol),
         lty=2,lwd=2)
segments(1.6,mean(controls$totserchol),
         2.4,mean(controls$totserchol),
         lty=2,lwd=2)

points(rep(1,length(cases$totserchol))+runif(length(cases$totserchol),-0.02,0.02),
       cases$totserchol,
       cex=1.5,col='sienna3',pch=16)

points(rep(2,length(controls$totserchol))+runif(length(controls$totserchol),-0.02,0.02),
       controls$totserchol,
       cex=1.5,col='slateblue2',pch=16)


# Part (c)
set.seed(123)
boxplot(cdata$totserchol~cdata$group,
        main='Cholesterol Levels in Males',
        names=c('2 days Post-MI','Controls'),
        col='lightgray')

segments(0.6,mean(cases$totserchol),
         1.4,mean(cases$totserchol),
         lty=2,lwd=2)
segments(1.6,mean(controls$totserchol),
         2.4,mean(controls$totserchol),
         lty=2,lwd=2)

points(rep(1,length(cases$totserchol))+runif(length(cases$totserchol),-0.02,0.02),
       cases$totserchol,
       cex=1.5,col='sienna3',pch=16)
points(rep(2,length(controls$totserchol))+runif(length(controls$totserchol),-0.02,0.02),
       controls$totserchol,
       cex=1.5,col='slateblue2',pch=16)

legend(1.4,360,
       legend=c('Cases',
                paste('N1 = ',dim(cases)[1]),
                paste('Mean = ',round(mean(cases$totserchol),2)),
                paste('Median = ',formatC(round(median(cases$totserchol),2),2,format='f')),
                'Data Values'),
       col=c(NA,NA,'black','black','sienna3'),
       lty=c(NA,NA,2,1,1),lwd=c(NA,NA,2,2,2),cex=.8)
legend(1.9,360,
       legend=c('Controls',
                paste('N2 = ',dim(controls)[1]),
                paste('Mean = ',round(mean(controls$totserchol),2)),
                paste('Median = ',formatC(round(median(controls$totserchol),2),2,format='f')),
                'Data Values'),
       col=c(NA,NA,'black','black','slateblue2'),
       lty=c(NA,NA,2,1,1),lwd=c(NA,NA,2,2,2),cex=.8)


# ----------------------------------------------------------
# Question 3: 
library(scales)

d1 <- density(cases$totserchol)
d2 <- density(controls$totserchol)
hist(cases$totserchol,freq=F,
     ylim=c(0,0.03),
     xlim=range(d1$x,d2$x),
     main='Cholesterol Levels in Males',
     xlab='Cholesterol levels',
     ylab='Proportion',
     col=alpha('sienna3',0.3))
hist(controls$totserchol,freq=F,add=T,
     col=alpha('slateblue2', 0.5))
lines(d1$x,d1$y,col='sienna3',lwd=3)
lines(d2$x,d2$y,col='slateblue2',lwd=3)
legend(250,0.028,
       legend=c('Cases',
                paste('N1 = ',dim(cases)[1]),
                paste('Mean = ',round(mean(cases$totserchol),2)),
                paste('Median = ',formatC(round(median(cases$totserchol),2),2,format='f')),
                'Data Values'),
       col=c(NA,NA,'black','black','sienna3'),
       lty=c(NA,NA,2,1,1),lwd=c(NA,NA,2,2,2),cex=.8)
legend(325,0.028,
       legend=c('Controls',
                paste('N2 = ',dim(controls)[1]),
                paste('Mean = ',round(mean(controls$totserchol),2)),
                paste('Median = ',formatC(round(median(controls$totserchol),2),2,format='f')),
                'Data Values'),
       col=c(NA,NA,'black','black','slateblue2'),
       lty=c(NA,NA,2,1,1),lwd=c(NA,NA,2,2,2),cex=.8)
box()


# ----------------------------------------------------------
# Question 4:

t.stat<- -3/(10/sqrt(30))
one.sided.pval <- pt(t.stat,df=29)
two.sided.pval <- 2*one.sided.pval

t.stat
one.sided.pval
two.sided.pval

# Part (a)
par(mfrow=c(1,2))
t.values <- seq(-3,3,0.001)     # Same support set as N(0,1)
t.density <- dt(t.values,29)

plot(t.values,t.density,type="l",lwd=2,
     main="One-Sided Paired T-test",
     ylab="density",xlab="t value")

x1<-c(-3,seq(-3,t.stat,by=0.001),t.stat)      
y1<-c(0,dt(seq(-3,t.stat,by=0.001),29),0)
polygon(x1,y1,col="slateblue2")               # Shade lower tail

plot(t.values,t.density,type="l",lwd=2,
     main="Two-Sided Paired T-test",
     ylab="density",xlab="t value")

x2<-c(-1*t.stat,seq(-1*t.stat,3,by=0.001),3)  
y2<-c(0,dt(seq(-1*t.stat,3,by=0.001),29),0)
polygon(x1,y1,col="slateblue2")               # Shade lower tail
polygon(x2,y2,col="slateblue2")               # Shade upper tail


# Part (b)
par(mfrow=c(1,2))
t.values <- seq(-3,3,0.001)     # Same support set as N(0,1)
t.density <- dt(t.values,29)

plot(t.values,t.density,type="l",lwd=2,
     main="One-Sided Paired T-test",
     ylab="density",xlab="t value")

x1<-c(-3,seq(-3,t.stat,by=0.001),t.stat)      
y1<-c(0,dt(seq(-3,t.stat,by=0.001),29),0)
polygon(x1,y1,col="slateblue2")               # Shade lower tail

text(2,0.38,cex=0.7,
     paste('P-Value =',round(one.sided.pval,3)))

plot(t.values,t.density,type="l",lwd=2,
     main="Two-Sided Paired T-test",
     ylab="density",xlab="t value")

x2<-c(-1*t.stat,seq(-1*t.stat,3,by=0.001),3)  
y2<-c(0,dt(seq(-1*t.stat,3,by=0.001),29),0)
polygon(x1,y1,col="slateblue2")               # Shade lower tail
polygon(x2,y2,col="slateblue2")               # Shade upper tail

text(2,0.38,cex=0.7,
     paste('P-Value =',round(two.sided.pval,3)))


# Part (c)
par(mfrow=c(1,2),oma=c(0,0,2,0))
t.values <- seq(-3,3,0.001)     # Same support set as N(0,1)
t.density <- dt(t.values,29)

plot(t.values,t.density,type="l",lwd=2,
     main="One-Sided Paired T-test",
     ylab="density",xlab="t value")

x1<-c(-3,seq(-3,t.stat,by=0.001),t.stat)      
y1<-c(0,dt(seq(-3,t.stat,by=0.001),29),0)
polygon(x1,y1,col="slateblue2")               # Shade lower tail

text(2,0.38,cex=0.7,
     paste('P-Value =',round(one.sided.pval,3)))

plot(t.values,t.density,type="l",lwd=2,
     main="Two-Sided Paired T-test",
     ylab="density",xlab="t value")

x2<-c(-1*t.stat,seq(-1*t.stat,3,by=0.001),3)  
y2<-c(0,dt(seq(-1*t.stat,3,by=0.001),29),0)
polygon(x1,y1,col="slateblue2")               # Shade lower tail
polygon(x2,y2,col="slateblue2")               # Shade upper tail

text(2,0.38,cex=0.7,
     paste('P-Value =',round(two.sided.pval,3)))

mtext('P-Value for Paired T-test',            # Add global title 
      cex=1.5,font=2,side=3,line=0,outer=TRUE)


# Part (d)
par(mfrow=c(1,2))
t.values <- seq(-3,3,0.001)     # Same support set as N(0,1)
t.density <- dt(t.values,29)

plot(t.values,t.density,type="l",lwd=2,
     main="One-Sided Paired T-test",
     ylab="density",xlab="t value")

t.one.sided <- qt(0.05,df=29)         # One-sided critical value 

x<-c(-3,seq(-3,t.one.sided,by=0.001),t.one.sided)      
y<-c(0,dt(seq(-3,t.one.sided,by=0.001),29),0)
polygon(x,y,col="slateblue2")           # Shade rejection region

plot(t.values,t.density,type="l",lwd=2,
     main="Two-Sided Paired T-test",
     ylab="density",xlab="t value")

t1.two.sided <- qt(0.025,df=29)      # Two-sided critical values 
t2.two.sided <- qt(0.975,df=29)

x1<-c(-3,seq(-3,t1.two.sided,by=0.001),t1.two.sided)      
y1<-c(0,dt(seq(-3,t1.two.sided,by=0.001),29),0)
x2<-c(t2.two.sided,seq(t2.two.sided,3,by=0.001),3)  
y2<-c(0,dt(seq(t2.two.sided,3,by=0.001),29),0)
polygon(x1,y1,col="slateblue2")         # Shade rejection region              
polygon(x2,y2,col="slateblue2")


# Part (e)
par(mfrow=c(1,2))
t.values <- seq(-3,3,0.001)     # Same support set as N(0,1)
t.density <- dt(t.values,29)

plot(t.values,t.density,type="l",lwd=2,
     main="One-Sided Paired T-test",
     ylab="density",xlab="t value")

t.one.sided <- qt(0.05,df=29)         # One-sided critical value 

x<-c(-3,seq(-3,t.one.sided,by=0.001),t.one.sided)      
y<-c(0,dt(seq(-3,t.one.sided,by=0.001),29),0)
polygon(x,y,col="slateblue2")           # Shade rejection region

segments(t.stat,0,t.stat,0.15,         # Plot observed test stat
         col='red',lty=2,lwd=3)

plot(t.values,t.density,type="l",lwd=2,
     main="Two-Sided Paired T-test",
     ylab="density",xlab="t value")

t1.two.sided <- qt(0.025,df=29)      # Two-sided critical values 
t2.two.sided <- qt(0.975,df=29)

x1<-c(-3,seq(-3,t1.two.sided,by=0.001),t1.two.sided)      
y1<-c(0,dt(seq(-3,t1.two.sided,by=0.001),29),0)
x2<-c(t2.two.sided,seq(t2.two.sided,3,by=0.001),3)  
y2<-c(0,dt(seq(t2.two.sided,3,by=0.001),29),0)
polygon(x1,y1,col="slateblue2")         # Shade rejection region              
polygon(x2,y2,col="slateblue2")

segments(t.stat,0,t.stat,0.15,         # Plot observed test stat
         col='red',lty=2,lwd=3)


# Part (f)
par(mfrow=c(1,2))
t.values <- seq(-3,3,0.001)     # Same support set as N(0,1)
t.density <- dt(t.values,29)

plot(t.values,t.density,type="l",lwd=2,
     main="One-Sided Paired T-test",
     ylab="density",xlab="t value")

t.one.sided <- qt(0.05,df=29)         # One-sided critical value 

x<-c(-3,seq(-3,t.one.sided,by=0.001),t.one.sided)      
y<-c(0,dt(seq(-3,t.one.sided,by=0.001),29),0)
polygon(x,y,col="slateblue2")           # Shade rejection region

segments(t.stat,0,t.stat,0.15,         # Plot observed test stat
         col='red',lty=2,lwd=3)

legend('topleft',                      # Add legend 
       legend='Observed Test Statistic',
       cex=0.6,col='red',lty=2,lwd=1.5)

plot(t.values,t.density,type="l",lwd=2,
     main="Two-Sided Paired T-test",
     ylab="density",xlab="t value")

t1.two.sided <- qt(0.025,df=29)      # Two-sided critical values 
t2.two.sided <- qt(0.975,df=29)

x1<-c(-3,seq(-3,t1.two.sided,by=0.001),t1.two.sided)      
y1<-c(0,dt(seq(-3,t1.two.sided,by=0.001),29),0)
x2<-c(t2.two.sided,seq(t2.two.sided,3,by=0.001),3)  
y2<-c(0,dt(seq(t2.two.sided,3,by=0.001),29),0)
polygon(x1,y1,col="slateblue2")         # Shade rejection region              
polygon(x2,y2,col="slateblue2")

segments(t.stat,0,t.stat,0.15,         # Plot observed test stat
         col='red',lty=2,lwd=3)


# Part (g)
par(mfrow=c(1,2),oma=c(0,0,2,0))
t.values <- seq(-3,3,0.001)     # Same support set as N(0,1)
t.density <- dt(t.values,29)

plot(t.values,t.density,type="l",lwd=2,
     main="One-Sided Paired T-test",
     ylab="density",xlab="t value")

t.one.sided <- qt(0.05,df=29)         # One-sided critical value 

x<-c(-3,seq(-3,t.one.sided,by=0.001),t.one.sided)      
y<-c(0,dt(seq(-3,t.one.sided,by=0.001),29),0)
polygon(x,y,col="slateblue2")           # Shade rejection region

segments(t.stat,0,t.stat,0.15,         # Plot observed test stat
         col='red',lty=2,lwd=3)

legend('topleft',                      # Add legend 
       legend='Observed Test Statistic',
       cex=0.6,col='red',lty=2,lwd=1.5)

plot(t.values,t.density,type="l",lwd=2,
     main="Two-Sided Paired T-test",
     ylab="density",xlab="t value")

t1.two.sided <- qt(0.025,df=29)      # Two-sided critical values 
t2.two.sided <- qt(0.975,df=29)

x1<-c(-3,seq(-3,t1.two.sided,by=0.001),t1.two.sided)      
y1<-c(0,dt(seq(-3,t1.two.sided,by=0.001),29),0)
x2<-c(t2.two.sided,seq(t2.two.sided,3,by=0.001),3)  
y2<-c(0,dt(seq(t2.two.sided,3,by=0.001),29),0)
polygon(x1,y1,col="slateblue2")         # Shade rejection region              
polygon(x2,y2,col="slateblue2")

segments(t.stat,0,t.stat,0.15,         # Plot observed test stat
         col='red',lty=2,lwd=3)

mtext('Rejection Region for Paired T-test',   # Add global title 
      cex=1.5,font=2,side=3,line=0,outer=TRUE)

# ----------------------------------------------------------
# End of Program
