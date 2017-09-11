Problem36 <- function(){

  require(data.table)  
  test <- data.table(dec = seq.int(
    from = 1, 
    to = (1e6 - 1), 
    by = 2))
  test <- test[Functions$IsPalindrome(dec)]
  
  test[,
    bin := gsub(
      pattern = '^0*',
      replacement ='',
      x =  paste(
        rev(as.integer(intToBits(dec))),
        collapse = ''
      )
    ),
    dec]
  
  test <- test[Functions$IsPalindrome(bin)]
  
  return(test[, sum(dec)])
    
}

# debug(Problem36)
# print(system.time(print(Problem36())))
