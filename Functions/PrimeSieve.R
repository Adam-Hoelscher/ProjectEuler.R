PrimeSieve<-function(to, from = 1){
  
  if (from > to){
    warning('Upper bound of sieve is below lower bound integer(0) returned')
    return(integer(0))
  }
  n <- to
  a <- c(F, rep(T, times = (n-1)))
  p <- 2
  
  while (p^2 <= n){
    j <- p^2
    while (j <= n){             
      a[j] <- F
      j <- (j+p)
    }
    p <- p+1
    while(!a[p]){p <- p+1}
  }
  
  temp <- (1:n)[a]
  temp <- temp[which(temp >= from)]
  return(temp)
  
}
