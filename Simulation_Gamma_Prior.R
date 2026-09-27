#######Exercise 4.21########


#Setup parameters
num <- 1040#Car  crashes
dt <- 2#Number of time periods
mu <- 700# Mean cost per crash
sd <-100# Standard deviation per crash

#Part 1
#Assume that the rate of crashes can be adequately modelled by a gamma distribution
#with parameters shape and rate
#shape=1040
#rate= 2 (years)
xseq <-  1:2000#Plausible range of values
num_crashes_prior <- dgamma(xseq,num,dt)
plot(xseq,num_crashes_prior,xlab="Number of crashes",ylab="Density",
     main="Number of crashes prior density")

#Part 2
#Derive a second order simulation for total cost

#Create function
sim_car_crash <- function(n){
  y <- c() #Empty vector to store results
  for (i in 1:n){
    num_crashes <- floor(rgamma(1,num,dt))
    y[i] <- sum(rnorm(num_crashes,mu,sd))
  }
  y
}

#Store results 
sim_results <- sim_car_crash(1000)

#Examine simulation outputs
hist(sim_results,main="Total cost per accidents",xlab="Cost Estimates")

#Descriptive Statistics
quantile(sim_results,c(0,0.025,0.5,0.975,1))
