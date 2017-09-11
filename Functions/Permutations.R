Permutation<-function(n, of){
  
  if (length(of)==1){of <- (1:of)}
  
  if (n>factorial(length(of))){
    warning('Requested permutation exceeds total numbers of permutations possible for given set')
  }
  
  remainder <- (n-1)
  temp <- integer(0)
  
  while (length(of)>0){
    f <- factorial(length(of)-1)
    item <- floor(remainder/f)+1
    remainder <- (remainder%%f)
    temp <- c(temp,of[item])
    of <- of[-item]
  }
  
  if (is.na(temp[1])){temp <- temp[-1]}
  
  return(temp)
}

PermNum<-function(p){
  
  temp <- 1
  
  while (length(p) > 0){
    n <- length(p[p<p[1]])
    temp <- temp + n*factorial(length(p)-1)
    p <- p[-1]
  }
  return(temp)
}
