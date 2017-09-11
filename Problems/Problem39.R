Problem39 <- function(){
  
  test <- data.table(expand.grid(
    a = 1:999,
    b = 1:999)
  )
  
  test <- test[b >= a]
  test[, c := (a^2 + b^2)^(1/2)]
  test <- test[c == as.integer(c)]
  
  test[, p := a + b + c]
  test <- test[p <= 1000]
  return(test[, .N, p][order(-N), p[1]])
}

# debug(Problem39)
# print(system.time(print(Problem39())))
