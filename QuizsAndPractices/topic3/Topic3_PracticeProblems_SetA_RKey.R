# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721 Practice Problems: Topic 3 Solution Key ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set A | Closed-Technology Practice Problems  

# --------------------------------------------------------
# Question 7

# Part (a)
x <- 5
if (x > 0) {print('X is non-negative')}

# Part (b)
x <- -1.5
if (x > 0) {print('X is non-negative')}

# Part (c)
x <- -1.5
if (x > 0) {print('X is non-negative')}
else {print('X is negative')}

# Part (d)
x <- -1.5
if (x > 0) {print('X is non-negative')
 } else {print('X is negative')}

# Part (e)
x <- 5
y <- NA
if (x > 0) {y <- 100}

# Part (f)
x <- 5
y <- NA
if (x > 0) {y <- 100}
y

# Part (g)
x <- NA
y <- NA
if (x > 0) {y <- 100}

# Part (h)
x <- seq(-5,15,by=5)
y <- NA
if (x > 0) {y <- 100}
y 

# Part (i)
x <- seq(-5,15,by=5)
ifelse(x > 0, 100, NA)


# --------------------------------------------------------
# Question 8

# Chunk1
X <- 10
Y <- NA
if (X < -5) { Y <- -5
} else if (X < 5) { Y <- 0
} else { Y <- 5}
Y

# Chunk2
X <- 10 
Y <- NA
if (X < -5) { Y <- -5 }
if (X < 5) {Y <- 0}
if (X >= 5) { Y <- 5}
Y

# --------------------------------------------------------
# Question 9

# Part (a) 
for (j in c('Go','Blue','Devils','Woot!')) { print(j) }

# Part (b)
for (i in 1:4) { i }; i

# Part (c)
counter <- 0
while (counter < 5) { counter <- counter + 1}

# Part (d) 
stack <- NULL
for (a in c('Group A','Group B')) {
  for (b in 1:3) { stack <- rbind(stack,data.frame(a,b))}}
stack

# Part (e)
a <- 10 
b <- -2
count <- 0 
while (a<20 | b > 4) {
  count <- count+1
  a <- a+count; print (a)
  b <- b+1           }
b

# Print (f)
for (i in 1:500) { 
  if (i%%100==0) {
    print(paste('This is interation ',i,'!',sep=''))}}

# --------------------------------------------------------
# Question 10

# Part (a)
counter <- 0
for (i in 1:10) { counter <- counter + 1 }

# Part (b)
counter <- 0
for (j in seq(0,50,by=10)) {
  for (k in LETTERS ) { counter <- counter + 1 }}

# Part (c)
counter <- 0
a <- 0 
while (a < 10) { a <- 2+a; counter <- counter + 1 }

# Part (d)
counter <- 0
a <- 0 
while (a > 10) { a <- 2+a; counter <- counter + 1 }

# Part (e)
counter <- 0
a <- 0 
while (a < 10) { a <- 2*a; counter <- counter + 1 }

# --------------------------------------------------------
# End of Program



