Problem38 <- function(){

  test <- data.table(value = 192:(9876+1))
  test[, stuff := as.character(value)]
  factor <- 2
  test[, keep := T]
  
  while (test[, max(nchar(stuff))] < 9){
    test[, stuff := paste0(stuff,
                           as.character(value * factor))]
    test[nchar(stuff) >= 9L, keep := Functions$IsPandigital(stuff), stuff]
    test <- test[keep == T]
    test <- test[value >= test[nchar(stuff) >= 9L, max(value)]]
    factor <- (factor + 1L)
  }
  return(test[value == test[,max(value)], as.integer(stuff)])
  
}

# debug(Problem38)
# print(system.time(print(Problem38())))
