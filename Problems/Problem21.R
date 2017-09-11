Problem21 <- function() {

  d <- function(n) {
    if (n <= 2) {
      return(1)
    } else {
      temp <- 1L
      for (x in 2:sqrt(n)){
        if (n %% x == 0){
          temp <- unique(c(temp, x, n/x))
        }
      }
      return(sum(temp))
    }
  }

#   debug(d)
  temp <- data.table(n1 = 1:9999)
  temp[,d1:=unlist(lapply(X = n1, FUN = d))]
  temp[,n2:=unlist(lapply(X = d1, FUN = d))]
  temp[,amicable:=(n1 == n2) & (n1 != d1)]
#   return(temp)
  return(temp[amicable == T, sum(n1)])
}

# debug(Problem21)
# print(system.time(print(Problem21())))
