GCD <- function(...) {
  
  n<-c(...)
  
  GCD1<-function(a,b){
    if (b==0){
      return(a)
    } else {
      return(GCD1(b,a%%b))
    }
  }
  
  if (length(n)==1){
    return(n)
  } else {
    return(GCD1(n[1],GCD(n[-1])))
  }
  
}

LCM <- function(...) {
  
  n<-c(...)
  
  LCM1<-function(a,b=NULL){
    return(a/GCD(c(a,b))*b)
  }
  
  if (length(n)==1){
    return (n)
  } else {
    return(LCM1(n[1],LCM(n[-1])))
  }
  
}
