Problem30 <- function() {

  power <- 5L
  temp <- data.table(n = 2L:(power * (9 ^ power)))
  len <- temp[,max(nchar(n))]

  temp[,n:=sprintf(fmt = paste('%0',len,'d',sep = ''), n)]
  digits <- unlist(x = lapply(X = temp[,n], FUN = strsplit, split = ''))
  digits <- matrix(data = as.integer(digits), ncol = len, byrow = T)

  temp[,s:=rowSums(x = (digits ^ power))]
  temp[,match:=(as.integer(n) == s)]
  temp <- temp[match == T]

  return(temp[,sum(s)])
}

# debug(Problem30)
# print(system.time(print(Problem30())))
