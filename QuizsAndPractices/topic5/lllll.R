# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721| Topic 5 Part 3: Plotting in R ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Code from Slides -------------------------------------------------------------

set.seed(111)
women <- rnorm(n=1000,mean=24.2,sd=3)
set.seed(222)
men <- rnorm(n=1000,mean=21.5,sd=2)

# ------------------------------------------------------------------------------
par(mfrow = c(1, 1))
par(mfrow = c(1, 2))

mean.w = round(mean(women), 2)
min.w = round(min(women),2)
max.w = round(max(women), 2)

hist(women, 
     breaks = 10,
     col = "red",
     main = "For women",
     xlab = paste("BMI women data", min.w, max.w),
     ylab = "frequency"
     
     )

text(19,200,labels=paste('mean=', mean.w) )
box()

mean.m = round(mean(men), 2)
min.m = round(min(men), 2)
max.m = round(max(men), 2)

hist(men, 
     breaks = 10,
     col = "blue",
     main = "For men",
     xlab = paste("BMI men", min.m, max.m),
     ylab = "frequency"
     
     )
text(19,170,labels=paste('mean=', mean.m) )
box()

unshuffled <- read.table("D:/GitHub/ADM_Statistic_Data/shuffled_local_ancestry_out/mixscore_shuffled0.out", header = F)

unshuffled <- unshuffled$V1

shuffled1 <- read.table("D:/GitHub/ADM_Statistic_Data/AdmixOut/1123_out/mixscore_shuffled1.out", header = F)

shuffled1 <- shuffled1$V1

shuffled2 <- read.table("D:/GitHub/ADM_Statistic_Data/AdmixOut/1123_out/mixscore_shuffled2.out", header = F)

shuffled2 <- shuffled2$V1

shuffled3 <- read.table("D:/GitHub/ADM_Statistic_Data/AdmixOut/1123_out/mixscore_shuffled3.out", header = F)

shuffled3 <- shuffled3$V1 


par(mfrow = c(1, 1))

pdf( file = 'mixscore ADM plot1.pdf')
par(mfrow = c(2, 2))

end <- length(unshuffled)
plot(x = 1:end, y = unshuffled[1:end], 
     xlab = paste('SNP range from 1:', end),
     ylab = 'mixscore ADM',
     main = 'Unshuffled',
     ylim = c(0, 20) )

plot(x = 1:end, y = shuffled1[1:end], 
     xlab = paste('SNP range from 1:', end),
     ylab = 'mixscore ADM',
     main = 'shuffled #1',
     ylim = c(0, 20)  )

plot(x = 1:end, y = shuffled2[1:end], 
     xlab = paste('SNP range from 1:', end),
     ylab = 'mixscore ADM',
     main = 'shuffled #2',
     ylim = c(0, 20)  )

plot(x = 1:end, y = shuffled3[1:end], 
     xlab = paste('SNP range from 1:', end),
     ylab = 'mixscore ADM',
     main = 'shuffled #3',
     ylim = c(0, 20)  )

dev.off()



###### without leaving 1st and last blocks unshuffled:

unshuffled <- read.table("D:/GitHub/ADM_Statistic_Data/shuffled_local_ancestry_out/mixscore_shuffled0.out", header = F)

unshuffled <- unshuffled$V1

shuffled <- read.table("D:/GitHub/ADM_Statistic_Data/Ashuffled_local_ancestry_out/mixscore_shuffled1.out", header = F)

shuffled1 <- shuffled1$V1

shuffled2 <- read.table("D:/GitHub/ADM_Statistic_Data/shuffled_local_ancestry_out/mixscore_shuffled2.out", header = F)

shuffled2 <- shuffled2$V1

shuffled3 <- read.table("D:/GitHub/ADM_Statistic_Data/shuffled_local_ancestry_out/mixscore_shuffled3.out", header = F)

shuffled3 <- shuffled3$V1 


par(mfrow = c(1, 1))

pdf( file = 'mixscore ADM plot1.pdf')
par(mfrow = c(2, 2))

end <- length(unshuffled)
plot(x = 1:end, y = unshuffled[1:end], 
     xlab = paste('SNP range from 1:', end),
     ylab = 'mixscore ADM',
     main = 'Unshuffled',
     ylim = c(0, 20) )

plot(x = 1:end, y = shuffled1[1:end], 
     xlab = paste('SNP range from 1:', end),
     ylab = 'mixscore ADM',
     main = 'shuffled #1',
     ylim = c(0, 20)  )

plot(x = 1:end, y = shuffled2[1:end], 
     xlab = paste('SNP range from 1:', end),
     ylab = 'mixscore ADM',
     main = 'shuffled #2',
     ylim = c(0, 20)  )

plot(x = 1:end, y = shuffled3[1:end], 
     xlab = paste('SNP range from 1:', end),
     ylab = 'mixscore ADM',
     main = 'shuffled #3',
     ylim = c(0, 20)  )

dev.off()


# ------------------------------------------------------------------------------
# End of Program