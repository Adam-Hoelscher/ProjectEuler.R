Problem34 <- function() {
  
  FactSum <- function(x) {
    x <- as.character(x)
    x <- strsplit(x = x, split = '')
    x <- unlist(x)
    x <- as.integer(x)
    x <- lapply(X = x, FUN = factorial)
    x <- unlist(x)
    return(sum(x))
  }

  temp <- data.table(x = 3:FactSum(factorial(9)-1))
  temp[,y:=unlist(lapply(X = x, FUN = FactSum))]
  temp[,curious:=(x == y)]
  temp <- temp[curious==T]
  
  return(temp[,sum(x)])
  
}

# debug(Problem34)
# print(system.time(print(Problem34())))
