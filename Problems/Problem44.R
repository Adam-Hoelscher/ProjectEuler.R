Problem44 <- function(){
  
  Pent <- function(n){Functions$FigurateNumber(n, s = 5)}
  IsPent <- function(p){Functions$IsFigurate(x = p, s = 5)}
  PentNums <- integer()
  `%BinIn%` <- Functions$`%BinIn%`
  
  k <- 1L
  repeat {
    Pk <- Pent(k)
    for (Pa in PentNums){
      Pj <- (Pk - Pa)
      Pz <- (Pj + Pk)
      # some testing showed that the standard %in% function performs better
      # than my custom binary search function. On the relatively small sizes
      # of these lists, it must be that being closer to the C code beats out
      # the algorithmic gains.
      # if (Pj %BinIn% PentNums & IsPent(Pz)){
      if (Pj %in% PentNums & IsPent(Pz)){
        return(Pk-Pj)
      }
    }
    PentNums <- c(PentNums, Pk)
    k <- k + 1L
  }
  
}
