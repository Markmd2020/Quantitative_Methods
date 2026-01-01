


  costs<-function(n1,n2){
    sd<-0.25
    y<-array(dim=c(n1,n2));
    for(i in 1:n1){
      mu<-runif(1,3.5,5);
      for(j in 1:n2){
        y[i,j]<-rnorm(1,mu,sd)}}
    y }
  
 
    

     
x<-seq(3,6,0.01)
cost2<-costs(10,1000)
      
plot(ecdf(cost2[1,]),do.p=FALSE,verticals=TRUE,xlab="Cost of building new bridge",main="Simulation")
par(new=T)
    for(i in 2:10){
      plot(ecdf(cost2[i,]),do.p=FALSE,verticals=TRUE,xlab="",main=" ",axes=F,lty=2)
      par(new=TRUE)
}
    