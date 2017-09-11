Problem57 <- function(){
  
  require(gmp)
  
  count <- 0
  
  n <- as.bigz(3)
  d <- as.bigz(2)
  
  for (x in 1L:1e3L){

    n0 <- n
    d0 <- d
    n <- n0 + d0 * 2
    d <- n0 + d0
    
    if(as.integer(log10(n)) > as.integer(log10(d))){count <- count + 1}
    
  }
  
  return (count)
  
}

# debug(Problem57)
# print(Problem57())
