Problem12<-function(){

  FactorCount<-function(x){return(2*length(which(0==x%%(1:sqrt(x)))))}

  i<-1
  temp<-1
  fc<-FactorCount(temp)

  while (fc<=500){
    i<-i+1
    temp<-temp+i
    fc<-FactorCount(temp)
  }
  return(temp)
}
