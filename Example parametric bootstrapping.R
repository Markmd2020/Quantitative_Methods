# Enter data 
wgt<-c(2.6,4.2,1.8,3.4,1.9,3.3,2.3,
       6.1,2.2,4.3,2.8,4.1,
       2.7,4.3,2.1,4.5,2.9,2.6,4.2,3.4)

# Main parametric function
pbs<-function(x,b){
  y<-c()
  m<-mean(x)
  s<-sd(x)
  for(i in 1:b){
    y[i]<-mean(rnorm(length(x),m,s))
  }
  y}

# Run the model and plot results
pwgt<-pbs(wgt,500)

plot(density(pwgt), xlab="Mean birthweight", main="Distribution of mean weights \n Parametric bootstrap")
