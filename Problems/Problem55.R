Problem55 <- function(){
  
  BigAdd <- function(x, y){
    z <- integer()
    carry <- 0
    for (i in 1L:max(length(x), length(y))){
      temp <- x[i] + y[i] + carry
      carry <- as.integer(temp/10L)
      z[i] <- temp - (carry * 10L)
    }
    if (carry > 0){
      i <- (i + 1L)
      z[i] <- carry
    }
    return(z)
  }
  
  IsLychrel <- function(x){
    for (y in 1L:50L){
      x <- BigAdd(x, rev(x))
      if (all(x == rev(x))) {
        return(F)
      }
    }
    return(T)
  }
  
  y <- 0
  
  for (x in 1L:9999L){
    v <- rev(as.integer(unlist(strsplit(as.character(x), split = ''))))
    if (IsLychrel(v)) {y <- (y+1)}
  }
  
  return(y)
}

# debug(Problem55)
# print(system.time(print(Problem55())))
