Problem51 <- function(){

  Primes <- Functions$PrimeSieve(999999L)
  Primes <- Primes[which(Primes > 9999L)]
  
  i <- 0L
  
  repeat {
    
    i <- (i + 1L)
    x <- Primes[i]
    
    digits <- unlist(strsplit(x = as.character(x), split = ''))
    q <- table(digits)
    RepDig <- names(q)[which(q==max(q))][1]
    
    if (max(q) != 3) {next}
    if (! (RepDig %in% as.character(0:2))) {next}
    
    PrimeCount <- 0
    for (y in (1L:9L)){
      NewDig <- as.character(y)
      test <- as.integer(paste0(gsub(
        pattern = RepDig,
        replacement = NewDig,
        x = digits),
        collapse = ''))
      if (Functions$IsPrime(test)){PrimeCount <- (PrimeCount + 1L)}
    }

    if (PrimeCount >= 8) return(x)
  }
}

# debug(Problem51)
# print(system.time(print(Problem51())))
