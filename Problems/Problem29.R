Problem29 <- function() {

  test <- 2:100

  FactorTable <- function(n) {
    if (n < 2) {
      return (data.table())
    }
    q <- Functions$PrimeFactors(n)
    temp <- data.frame(lapply(X = split(x = q, f = q),
                              FUN = length))
    setDT(temp)
    return(temp)
  }

  factors <- rbindlist(lapply(X = split(x = test, f = test),
                              FUN = FactorTable),
                       fill = T)
  
  temp <- rbindlist(lapply(X = test,
                           FUN = function(x){return (factors*x)}),
                    fill = T)
                  
  return(dim(unique(temp))[1])
  
}

# debug(Problem29)
# print(system.time(print(Problem29())))
