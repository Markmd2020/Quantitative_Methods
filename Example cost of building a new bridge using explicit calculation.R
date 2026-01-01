coste<-function(n1){
  x<-seq(2.5,6,0.01)
  sd<-0.25
  y<-array(dim=c(n1,length(x)));
  for(i in 1:n1){
    mu<-runif(1,3.5,5);
    for(j in 1:length(x)){
      y[i,j]<-pnorm(x[j],mu,sd)}}
  y }

x<-seq(2.5,6,0.01)
cost<-coste(10)
plot(x,cost[1,],xlab="Cost of building new bridge",main=" ",ylab="Cumulative probability",type="l")
lines(x,cost[2,],lty=2)
lines(x,cost[3,],lty=3)
lines(x,cost[4,],lty=4)
lines(x,cost[5,],lty=5)
lines(x,cost[6,],lty=6)
lines(x,cost[7,],lty=7)
lines(x,cost[8,],lty=8)
lines(x,cost[9,],lty=9)
lines(x,cost[10,],lty=10)
