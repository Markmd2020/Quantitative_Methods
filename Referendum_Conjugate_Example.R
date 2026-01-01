#####Exercise 4.15:Referendum Example#####

#Question 1: What is the referendum likely outcome
#Step 1:Calculate Prior
calc.b<-function(a, mode){
  b<-2-a+(a-1)/mode
  print(b)
}

calc.b(a=95, mode=0.6)

x.range<-seq(0,1,0.01)
a<-95 #a chosen large to give peaky distribution
b<-64 #b calculated from calc.b function to give mode of 0.6
prior<-dbeta(x.range, a, b)
plot(x.range, prior, xlab="Proportion voting for Scotland to leave UK",
     type="l", ylab="Density", lwd=2, col="blue", lty=2, 
     main="Political correspondent's prior belief")

#Step 2:Calculate Likelihood
x.range <- seq(0,1,by=0.01)
n <- 820
s <- 451
likelihood<-dbinom(s, n, x.range)
plot(x.range, likelihood, xlab="Proportion of Pro Independence", 
     type="l", ylab="Density", col="blue", lty=2, 
     main="Likelihood from the poll results")

#Step 3:Calculate Posterior
posterior <-dbeta(x.range,a+s,b+n-s)
plot(x.range, prior/max(prior), xlab="Proportion of Pro Independence",
     type="l",ylab="Density", lwd=2, col="blue", lty=2,
     main="Pro Independence prior belief")
lines(x.range, likelihood/max(likelihood), col="purple", lty=4)
lines(x.range, posterior/max(posterior), col="black", lty=1)

legend("topleft", c("Prior", 
                    "Likelihood", "Posterior"),
       col=c("blue", "purple", "black"),lty=c(2,4,1), cex=0.8)

#Assess credible intervals
quantile(qbeta(x.range,a+s,b+n-s),c(0,0.025,0.5,0.975,1))

#Question 2: Is the political correspondent likely to be correct?
mean(rbeta(1000,a+s,b+n-s)>0.5)