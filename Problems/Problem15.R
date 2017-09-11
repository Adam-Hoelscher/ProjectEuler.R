Problem15 <- function() {
  temp <- matrix(rep.int(x = 1, times = 21 * 21),
                 nrow = 21,
                 ncol = 21)
  for (r in 2:21) {
    for (c in 2:21) {
      temp[r,c] <- temp[r-1,c] + temp[r,c-1]
    }
  }
  return(temp[21,21])
}
