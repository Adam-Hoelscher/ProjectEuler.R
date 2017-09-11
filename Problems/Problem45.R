Problem45 <- function(){
  
  n <- 143L
  h <- -Inf

  repeat {
    n <- n + 1L
    h <- Functions$FigurateNumber(n = n, s = 6)
    if (Functions$IsFigurate(x = h, s = 5)){
      return(h)
    }
  }
  
  return(numbers[, unique(value)])
}

# debug(Problem45)
# print(system.time(print(Problem45())))
