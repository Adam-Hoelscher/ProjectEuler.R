Problem3<-function(){
  x<-600851475143
  d<-2
  while (d<x){
    if (x %% d == 0){
      x <- x/d
    } else {
      d <- d+1
    }
  }
  return (x)
}
