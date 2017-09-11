Problem2<-function(){
  Fib<-c(1,1)
  while (Fib[1]+Fib[2]<4000000){
    Fib<-c(Fib[1]+Fib[2],Fib)
  }
  temp<-sum(Fib[Fib %% 2 == 0])
  return(temp)
}
