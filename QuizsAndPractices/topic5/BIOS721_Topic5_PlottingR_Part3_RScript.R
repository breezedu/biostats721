
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721| Topic 5 Part 3: Plotting in R ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Code from Slides -------------------------------------------------------------

set.seed(111)
women <- rnorm(n=1000,mean=24.2,sd=3)
set.seed(222)
men <- rnorm(n=1000,mean=21.5,sd=2)

# ------------------------------------------------------------------------------
head(women)

## round(x, digits = 0)


min.w = round(min(women), digits = 2)
max.w = round(max(women), digits = 2)
min.m = round(min(men), digits = 2)
max.m = round(max(men), digits = 2)

wx.lab <- paste('BMI value range = ', min.w, 'to', max.w)
mx.lab <- paste('BMI value range = ', min.m, 'to', max.m)

par(mfrow=c(1,2))
## plot women
hist(women,
     breaks = 20,
     freq = T,
     col='purple',
     main = 'for women',
     xlab = wx.lab
    )
mean.w = round(mean(women), digits = 2)
text(20,115,labels= (paste('Women mean = ', mean.w) ))
box()

## plot men
hist(men,
     breaks = 20,
     freq = T,
     col='purple',
     main = 'for men',
     xlab = mx.lab
)
mean.m = round(mean(men), digits = 2)


text(x=16, y=115,labels= (paste('Men mean = ', mean.m) ))
box()



# ------------------------------------------------------------------------------
# End of Program

