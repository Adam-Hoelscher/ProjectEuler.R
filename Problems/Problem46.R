Problem46 <- function(){
  
  Primes <- 2
  x <- 1
  
  repeat {
    x <- (x+2)
    if (Functions$IsPrime(x)){
      Primes <- c(Primes, x)
    } else {
      y <- sqrt((x - Primes)/2)
      test <- any(y == round(y, 0))
      if (!test){return(x)}
    }
  }

}

# debug(Problem46)
# print(system.time(print(Problem46())))
