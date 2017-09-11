Problem58 <- function(){

  pos   <- 1L
  width <- 2L
  prime <- 0L
  total <- 1L
  
  repeat {
    for (i in 1L:3L){
      pos <- (pos + width)
      if (Functions$IsPrime(pos)) prime <- prime + 1L
    }
    #we know this one isn't prime; it's a perfect square
    pos <- (pos + width)
    total <- total + 4L
    if (prime * 10 < total) return (width + 1L)
    width <- (width + 2L)
  }
  
}

# Problem58 <- function(){
# 
#   string <- c(3L, 5L, 7L, 9L)
#   step   <- 2L
#   testPs <- Functions$IsPrime(string)
#   # check  <- sum(testPs)/length(testPs)
#   
#   while ( (10*sum(testPs)) > length(testPs) ) {
#     step <- (step + 2L)
#     new  <- max(string) + step * (1L:4L)
#     string <- c(string, new)
#     testPs <- c(testPs, Functions$IsPrime(new))
#     # check  <- sum(testPs)/length(testPs)
#   } 
#   
#   return(step + 1L)
# }

# debug(Problem58)
# print(system.time(print(Problem58())))
