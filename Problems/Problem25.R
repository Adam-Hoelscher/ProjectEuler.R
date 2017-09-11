Problem25 <- function() {

  BigIntAdd <- function(i1, i2) {
    places <- max(c(length(i1),length(i2)))
    carry <- 0
    count <- 1
    temp <- integer(0)
    while ((carry != 0) | (count <= places)) {
      value <- sum(i1[count],i2[count],carry,na.rm=T)
      temp <- c(temp, (value %% 10))
      carry <- as.integer(value / 10)
      count <- (count + 1)
    }
    return(temp)
  }
  
  f1 <- 0L
  f2 <- 1L
  f3 <- BigIntAdd(f1,f2)
  index <- 2L

  while (length(f3) < 1000) {
    f1 <- f2
    f2 <- f3
    f3 <- BigIntAdd(f1,f2)
    index <- (index + 1)
  }
  
  return(index)
}

# debug(Problem25)
# print(Problem25())
