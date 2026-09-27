x.range<-seq(0,1,0.01)

a<-70

calc.b<-function(a, mode){
  b<-2-a+(a-1)/mode
  print(b)
}

calc.b(a=70, mode=0.45)

b<-85
prior<-dbeta(x.range, a, b)
plot(x.range, prior, xlab="Proportion voting SNP", type="l",
     ylab="Density", col="blue", lty=2)





n<-1026
s<-414

likelihood<-dbinom(s, n, x.range) 
plot(x.range, likelihood, xlab="Proportion voting SNP", type="l",
     ylab="Density", col="purple", lty=4)


posterior<-dbeta(x.range,a+s,b+n-s)
plot(x.range, posterior, xlab="Proportion voting SNP", type="l", 
     ylab="Density", col="black", lty=1)



plot(x.range, prior/max(prior), xlab="Proportion voting SNP", 
     type="l", ylab="Scaled density", lwd=2, col="blue", lty=2)
lines(x.range,likelihood/max(likelihood), lwd=2, col="purple", lty=4)
lines(x.range,posterior/max(posterior), lwd=2, col="black", lty=1)
legend("topright", c("Prior: Beta(70,85)", 
                     "Likelihood: Binomial(1026, 0.403)",
                     "Posterior: Beta(484, 697)"), 
       col=c("blue", "purple", "black"), lty=c(2,4,1), cex=0.8, lwd=2)

crI.lower<-qbeta(0.025,a+s,b+n-s)
crI.upper<-qbeta(0.975, a+s,b+n-s)
median<-qbeta(0.5, a+s,b+n-s)

#####
#recreating the results with a smaller sample size

n<-103
s<-41
a<-70
b<-85

prior<-dbeta(x.range, a, b) 
likelihood<-dbinom(s, n, x.range) 
posterior<-dbeta(x.range,a+s,b+n-s)

plot(x.range, prior/max(prior), xlab="Proportion voting SNP", 
     type="l", ylab="Scaled density", lwd=2, col="blue", lty=2)
lines(x.range,likelihood/max(likelihood), lwd=2, col="purple", lty=4)
lines(x.range,posterior/max(posterior), lwd=2, col="black", lty=1)
legend("topright", c("Prior: Beta(70,85)", 
                     "Likelihood: Binomial(103, 0.403)",
                     "Posterior: Beta(111, 147)"), 
       col=c("blue", "purple", "black"), lty=c(2,4,1), cex=0.8, lwd=2)
       
##smaller sample size with different prior belief       
a<-70
calc.b(a=70, mode=0.15)
b<-392
n<-103
s<-41

prior<-dbeta(x.range, a, b)       
likelihood<-dbinom(s, n, x.range) 
posterior<-dbeta(x.range,a+s,b+n-s)


plot(x.range, prior/max(prior), xlab="Proportion voting SNP", 
     type="l", ylab="Scaled density", lwd=2, col="blue", lty=2)
lines(x.range,likelihood/max(likelihood), lwd=2, col="purple", lty=4)
lines(x.range,posterior/max(posterior), lwd=2, col="black", lty=1)
legend("topright", c("Prior: Beta(70,392)", 
                     "Likelihood: Binomial(103, 0.403)",
                     "Posterior: Beta(111, 454)"), 
       col=c("blue", "purple", "black"), lty=c(2,4,1), cex=0.8, lwd=2)