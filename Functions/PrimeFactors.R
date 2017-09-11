PrimeFactors <- function(x) {
  if (x < 2) {return (NA)}
  temp <- integer(0)
  d <- 2
  while (x > 1) {
    if (d > sqrt(x)) {
      temp <- c(temp, x)
      x <- 1
    }
    if ((x %% d) == 0) {
      temp <- c(temp, d)
      x <- (x / d)
    } else {
      d <- (d + 1)
    }
  }
  return(temp)
}
