Problem27 <- function() {

  CountPrimes <- function (a, b){
    n <- 0
    temp <- 0
    f <- function(n) {return(n^2 + a*n + b)}
    while (Functions$IsPrime(f(n))) {
      n <- (n + 1)
      temp <- (temp + 1)
    }
    return (temp)
  }

# start with all values as set by problem
  a <- -999:999  
  b <- -999:999

# if b is not prime then f(0) will not be prime  
  b <- b[Functions$IsPrime(b)]

# if f(n) is even then f(n+2), f(n+4) are also even
# since f is quadratic it cannot be true that
# f(n) = f(n+2) = f(n+4)
# one of f(n), f(n+2), f(n+4) is even and not = 2; it is composite
# if f(0) was even and prime then b = 2 and one of f(2), f(4) is composite
# b = 2 creates too short of a list
  stopifnot(b[1]==2) # sorted list; 2 is the smallest prim
  b <- b[-1]

# if b != 2 and b is prime then b is odd
# if b is odd and a is even then f(1), f(3), f(5) are even
# one of f(1), f(3), f(5) is even and not = 2; it is composite
# a being even creates too short of a list
  a <- a[!(a %% 2 == 0)]

  test <- data.table(expand.grid(a = a, b = b))

# if a and b are both negative then f(0) is not prime (negative)
  test <- test[!((a < 0) & (b < 0))]

  test[,Count:=mapply(FUN = CountPrimes, a = a, b = b, USE.NAMES = T)]
  test <- test[Count == test[,max(Count)]]
  return(test[,a * b])
}

# debug(Problem27)
# print(system.time(print(Problem27())))
