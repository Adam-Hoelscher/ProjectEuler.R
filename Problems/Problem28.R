Problem28 <- function() {
  temp <- 1
  width <- 1
  place <- 1
  while (width < 1001) {
    step <- width + 1
    temp <- sum(temp,
                place * 4,
                step * 10)
    place <- place + step * 4
    width <- (width + 2)
  }
  return(temp)
}

# debug(Problem28)
# print(system.time(print(Problem28())))
