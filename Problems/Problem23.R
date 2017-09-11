Problem23 <- function() {
  
  listfactors <- function(n) {
    temp <- 1L
    if (n > 2) {
      for (x in 2:sqrt(n)) {
        if (n %% x == 0) {
          temp <- c(temp,x,n/x)
        }
      }
    }
    return(unique(temp))
  }
  
  sumfactors <- function(n) {
    return(sum(listfactors(n)))
  }

  isabundant <- function(n) {
    return(sumfactors(n) > n)
  }
  
  abundants <- which(unlist(lapply(X = 1:28124,
                                   FUN = isabundant)))
  
  temp <- data.table(expand.grid(n1 = abundants, n2 = abundants))
  temp <- temp[n2 >= n1]
  temp[,n3:=(n1 + n2)]
  return(sum(which(!(1:28123 %in% temp[,n3]))))

}

# debug(Problem23)
# print(Problem23())
