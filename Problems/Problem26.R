Problem26 <- function() {
  
  RepCount <- function(n) {
    base <- 1
    loop <- T
    remainders <- integer(0)
    while (loop) {
      remainders <- c(remainders, base %% n)
      base <- (10 * (base %% n))
      loop <- (length(unique(remainders))==length(remainders))
    }
    remainders <- rev(remainders)
    return(which(remainders[-1] == remainders[1]))  }
  
  test <- data.table(n = 1:999)
  test[,Count:=unlist(lapply(X = n, FUN = RepCount))]
  return(test[Count == test[,max(Count)],n])
}

# debug(Problem26)
# print(Problem26())
