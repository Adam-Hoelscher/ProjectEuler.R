Problem52 <- function(){
  
  DigTest <- function(num){
    baseDigits <- sort(unlist(strsplit(x = as.character(num   ), split = '')))
    for (m in 6L:2L){
      multDigits <- sort(unlist(strsplit(x = as.character(num*m), split = '')))
      if (length(baseDigits) != length(multDigits)) return(F)
      if (!all(baseDigits == multDigits)) return (F)
    }
    return(T)
  }
  
  x <- 125874L
  while (!DigTest(x)){
    x <- (x + 1L)
  }
  
  return(x)
  
}

# debug(Problem52)
# print(system.time(print(Problem52())))
