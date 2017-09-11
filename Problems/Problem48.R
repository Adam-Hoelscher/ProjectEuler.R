Problem48 <- function(){
  
  Last10 <- rep(x = 0, times = 10)
  
  for (i in 1:1000){
    
    product <- c(1, rep(x = 0, times = 9))
    for (x in 1:i){
      carry <- 0
      for (digit in 1:10){
        temp <- product[digit] * i + carry
        product[digit] <- temp %% 10
        carry <- as.integer(temp/10)
      }
    }
    
    carry <- 0
    for (digit in 1:10){
      temp <- Last10[digit] + product[digit] + carry
      Last10[digit] <- temp %% 10
      carry <- as.integer(temp/10)
    }
    
  }

    return(paste0(rev(Last10), collapse = ''))
}

# debug(Problem48)
# print(system.time(print(Problem48())))
