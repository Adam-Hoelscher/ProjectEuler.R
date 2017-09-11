IsPrime <- function(x) {
  
  P1<-function(x){
    if (x < 2) {
      return(F)
    } else if (x == 2) {
      return(T)
    } else {
      return(!as.logical(max(x%%(2:sqrt(x))==0)))
    }
  }
  
  return(unlist(lapply(X = x, FUN = P1)))
}
