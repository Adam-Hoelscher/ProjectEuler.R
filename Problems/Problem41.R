Problem41 <- function(){

#   no 8 or 9 pandigital number can be prime 
#   the sum of their digits is a multiple of 3
  for (x in 7:2){
    for (y in factorial(x):1){
      test <- Functions$Permutation(y, (1:x))
      if (!(test[x] %in% c(0, 2, 4, 5, 6, 8))){
      test <- as.integer(
        paste0(
          test,
          collapse = ''
        )
      )
      if (Functions$IsPrime(test)){
        return(test)
      }}
    }
  }
  
}

# debug(Problem41)
# print(system.time(print(Problem41())))
