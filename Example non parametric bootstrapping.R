# Enter the data 
wgt<-c(2.6,4.2,1.8,3.4,1.9,3.3,2.3,
       6.1,2.2,4.3,2.8,4.1,
       2.7,4.3,2.1,4.5,2.9,2.6,4.2,3.4)

# Main non-parametric function
npbs<-function(x,b){
  y<-c()
  for(i in 1:b){
    y[i]<-mean(sample(x,length(x),replace=TRUE))
  }
  y}

# Run model and plot results 
npwgt<-npbs(wgt,500)

plot(density(npwgt), xlab="Mean birthweight", 
     main="Distribution of mean weights \n Non-parametric bootstrap")

mean(npwgt)
quantile(npwgt, probs=seq(0,1,0.025))
