Problem24 <- function() {
  perm <- 1e6
  digits <- 0:9
  temp <- integer(0)
  len <- length(digits)
  while (len > 0) {
    f <- factorial(len-1)
    index <- as.integer((perm-1)/f)
    perm <- perm - (index * f)
    temp <- c(temp,digits[index+1])
    digits <- digits[-(index+1)]
    len <- length(digits)
  }
  return(paste(temp,collapse=''))
}

# debug(Problem24)
# print(Problem24())
