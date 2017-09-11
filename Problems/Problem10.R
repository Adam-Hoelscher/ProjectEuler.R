Problem10<-function(){
  temp<-data.table(x=1:(2e6))
  return(temp[Functions$IsPrime(x),sum(as.double(x))])
}
