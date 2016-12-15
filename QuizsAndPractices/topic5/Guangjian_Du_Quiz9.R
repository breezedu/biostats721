# ~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Quiz 9 ~
# ~~~~~~~~~~~~~~~~~~~

set.seed(987)
d <- rchisq(1000,10)

# --------------------------------------------------------------
# Question 1
boxplot(d,
        horizontal = T)

abline(v = mean(d), col = "red", lwd = 4)


# --------------------------------------------------------------
# Question 2

hist(d, 
     main = 'Histogram of d',
     xlab = 'd'
     )
abline(v = mean(d), col = "red", lwd = 4)
# --------------------------------------------------------------
# Question 3

par(mfrow = c(2, 1))
boxplot(d,
        horizontal = T)

abline(v = mean(d), col = "red", lwd = 4)
box()


hist(d, 
     main = 'Histogram of d',
     xlab = 'd'
)
abline(v = mean(d), col = "red", lwd = 4)
box()

# --------------------------------------------------------------
# End of Program