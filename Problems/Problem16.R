Problem16 <- function() {
  digits <- 1L
  for (x in 1L:1000L) {
    p <- 0L
    carry <- 0L
    loop <- T
    while (loop) {
      p <- (p + 1)
      if (p > length(digits)) {
        digits <- c(digits,0L)
      }
      placevalue <- (digits[p] * 2L + carry)
      digits[p] <- placevalue %% 10L
      carry <- as.integer(placevalue/10L)
      loop <- (p < length(digits) || (carry != 0))
    }
  }
  return(sum(digits))
}

# debug(Problem16)
# print(Problem16())
