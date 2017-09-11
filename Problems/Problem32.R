Problem32 <- function() {

  temp <- integer(0)

  for (x in 1:(factorial(9) / factorial(4))) {
    perm <- Functions$Permutation(n = (x * factorial(4)), of = 9)

    # check the 1 digit × 4 digit
    LS1 <- as.integer(paste(perm[1:1],collapse = ''))
    LS2 <- as.integer(paste(perm[2:5],collapse = ''))
    RS <- LS1 * LS2
    if (Functions$IsPandigital(LS1, LS2, RS, n = 9)) {
      temp <- c(temp, RS)
    }
  
    # check the 2 digit × 3 digit
    LS1 <- as.integer(paste(perm[1:2],collapse = ''))
    LS2 <- as.integer(paste(perm[3:5],collapse = ''))
    RS <- LS1 * LS2
    if (Functions$IsPandigital(LS1, LS2, RS, n = 9)) {
      temp <- c(temp, RS)
    }
  }

  return(sum(unique(temp)))

}

# debug(Problem32)
# print(system.time(print(Problem32())))
