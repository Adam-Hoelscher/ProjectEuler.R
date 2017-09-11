Problem47 <- function(){

  x <- 1
  repeat {
    x <- (x+1)
    if (length(unique(Functions$PrimeFactors(x))) == 4){
      count <- (count+1)
    } else {
      count <- 0
    }
    if (count == 4){return(x - 4 + 1)}
  }
}

# print(system.time(print(Problem47())))
