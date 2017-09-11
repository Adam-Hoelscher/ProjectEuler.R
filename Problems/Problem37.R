Problem37 <- function(){
  
  test <- data.table(value = 10:(1e6))
  test <- test[Functions$IsPrime(value)]
  # test[, prime := T]
  test[, `:=`(left  = value,
              right = value)]
  test[, dig := nchar(value)]
  
  while (test[,max(dig)] > 1){
    
    test[dig != 1,
         `:=`(left  = as.integer(substr(left,
                                        1,
                                        nchar(left) - 1)),
              right = as.integer(substr(right,
                                        2,
                                        nchar(right)))
         )]
    
    test[dig != 1,
         `:=` (lPrime = Functions$IsPrime(left),
               rPrime = Functions$IsPrime(right))]

    test <- test[lPrime & rPrime]
    test[dig != 1, dig := dig - 1L]
  }
  
  #   test[, keep1 := T]
  #   test[, keep0 := F]
  # print(test[,(value)])
  return(test[,sum(value)])    
  
}

# debug(Problem37)
# print(system.time(print(Problem37())))
