#Exercise 2.5#

#Inversion method example#

#Set seed to ensure reproducibility
set.seed(135)

#Setup lambda paramater
lam <- 5

#Step 1: Generate random variable#
u <- runif(1)
#Step 2: Calculate  x
x <- -log(1-u)/lam
#Step 3: Return x
x

#Wrap into a function that enables n iterations
inv_method <- function(lam=5,n=1000){
  x <- numeric(n)
  for (i in 1:n){
    u <- runif(1)
    x[i] <- -log(1-u)/lam
  }
  x
}

#Test function with default parameters
test <-inv_method()
head(test)