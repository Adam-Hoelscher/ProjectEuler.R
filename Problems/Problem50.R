Problem50 <- function(){
  
  ubound <- 1e6
  temp <- list(value = 2, len = 1)
  
  Primes <- Functions$PrimeSieve(ubound)

  for (i in 1:(length(Primes)-1)){
    index <- temp$len
    if ((i + temp$len - 1) > length(Primes)) break
    test <- sum(Primes[i:(i + temp$len - 1)])
    while (test < ubound){
      if (test %in% Primes){
        temp <- list(value = test, len = index)
      }
      test <- (test + Primes[i + index])
      index <- (index+1)
    }
  }

  return(temp$value)
  
}

# debug(Problem50)
# print(system.time(print(Problem50())))

Problem50b <- function(){
  
  ubound <- 1e6
  temp <- list(value = 2, len = 1)
  
  Primes <- Functions$PrimeSieve(ubound/2)
  
  for (p in Primes){
    subPrimes <- Primes[which(Primes >= p)]
    index <- temp$len
    test <- sum(subPrimes[1:index])
    while(test < ubound & index < (length(subPrimes))){
      if (Functions$IsPrime(test)){
        temp <- list(value = test, len = index)
      }
      index <- (index+1)
      test <- sum(subPrimes[1:index])
    }
  }
  print(temp$len)
  return(temp$value)
  
}
