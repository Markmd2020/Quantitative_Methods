
#Part 1
#Assume a beta prior but need to choose values of a, b
#Want quite peaked
#Most of the distribution above 50%
#Choose value of a, and calculate value of b using the
#calc.b function (on myplace) 
  

calc.b<-function(a, mode){
  b<-2-a+(a-1)/mode
  print(b)
}

calc.b(a=100, mode=0.6)

x.range<-seq(0,1,0.01)
a<-100 #a chosen large to give peaky distribution
b<-67 #b calculated from calc.b function to give mode of 0.6
prior<-dbeta(x.range, 100, 67)
plot(x.range, prior, xlab="Proportion voting for Scotland to leave UK",
     type="l", ylab="Density", lwd=2, col="blue", lty=2, 
     main="Political correspondent's prior belief")

#likelihood binomial n=820 s=369 will vote to leave UK, 45%

n<-820
s<-369
likelihood<-dbinom(s, n, x.range)
plot(x.range, likelihood, xlab="Proportion voting to leave UK", 
     type="l", ylab="Density", col="blue", lty=2, 
     main="Likelihood from the poll results")

posterior<-dbeta(x.range,a+s,b+n-s)

plot(x.range, prior/max(prior), xlab="Proportion voting to leave UK",
     type="l",ylab="Density", lwd=2, col="blue", lty=2,
     main="Political correspondent's prior belief")
lines(x.range, likelihood/max(likelihood), col="purple", lty=4)
lines(x.range, posterior/max(posterior), col="black", lty=1)

legend("topright", c("Prior: Beta(100, 67)", 
      "Likelihood: Binomial(820, 0.45)", "Posterior: Beta(469,518)"),
       col=c("blue", "purple", "black"),lty=c(2,4,1), cex=0.8)


crI.lower<-qbeta(0.025,469,518)
crI.upper<-qbeta(0.975,469,518)
median<-qbeta(0.5,469,518)

#Evidence suggests that the vote will be close and it is too uncertain 
#to predict one way or another (50% is in the CrI)


#Part 2
#Is the political correspondent likely to be correct?
  
sim.result<-rbeta(1000,a+s,b+n-s)
table(sim.result>=0.5)
64/1000


