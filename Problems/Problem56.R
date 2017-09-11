Problem56 <- function(){

  require(gmp)
  
  q <- 0
  t <- 0

  for (a in 99L:1L){
    temp <- 1
    for (b in 99L:1L){
      temp <- pow.bigz(a, b)
      temp <- as.character(temp)
      temp <- unlist(strsplit(temp, ''))
      temp <- as.integer(temp)
      if (9*length(temp) < q) break
      q <- max(q, sum(temp))
    }
  }
  
  return(q)
}

# debug(Problem56)
# print(system.time(print(Problem56())))
