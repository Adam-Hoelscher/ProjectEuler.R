Problem33 <- function() {
  
  test <- function(a, b) {

    q0 <- (a / b)

    a <- unlist(strsplit(x = as.character(x = a), split = ''))
    b <- unlist(strsplit(x = as.character(x = b), split = ''))

    for (x in 2:1) {
      for (y in 2:1) {
        if (a[x] == b[y]) {
          a <- a[-x]
          b <- b[-y]
          return(q0 == (as.integer(a)/as.integer(b)))
        }
      }
    }

# return FALSE if we couldn't find a naive simplified fraction
    return(F)
  }
  
  temp <- data.table(expand.grid(a = 10:99,
                                 b = 10:99))
  
  temp <- temp[!(a >= b)]
  temp <- temp[!((a %% 10 == 0) & (b %% 10 == 0))]
  temp[,curious:=mapply(FUN = test, a = a, b = b)]
  temp <- temp[curious == T]

  a <- Functions$PrimeFactors(temp[,prod(a, na.rm = T)])
  b <- Functions$PrimeFactors(temp[,prod(b, na.rm = T)])

  x <- 1
  y <- 1
  while ((x <= length(a)) & (y <= length(b))) {
    if (a[x] == b[y]) {
      a <- a[-x]
      b <- b[-y]
    } else if (a[x] > b[y]) {
      y <- (y + 1)
    } else {
      x <- (x+1)
    }
  }

  return(prod(b))
}

# debug(Problem33)
# print(system.time(print(Problem33())))
