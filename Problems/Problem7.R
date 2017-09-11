Problem7<-function(){
  c<-0
  x<-1
  while (c<10001){
    x<-x+1
    if (Functions$IsPrime(x)) {c<-c+1}
  }
  return(x)
}
