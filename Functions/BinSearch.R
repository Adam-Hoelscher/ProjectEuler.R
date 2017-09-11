BinMatch <- function(x, table, nomatch = NA_integer_){
  
  l <- 1L
  h <- length(table)
  
  if (h == 1){
    return(x == table)
  }

  if (length(x) > 1){
    return(sapply(
      X = x,
      FUN = BinMatch,
      table = table,
      nomatch = nomatch))
  }

  if (x < table[l] || x > table[h]) return(nomatch)
  while (l != h){
    n <- as.integer((l+h)/2)
    if (table[n] == x) {
      return(n)
    } else if (table[n] > x) {
      h <- n
    } else if (table[n] < x) {
      # if l is already equal to n it's because floor((l+h)/2) = l
      # this implies that l+1 = h, and there is no int between l and h
      # therefore x is not in table and we return the nomatch value
      if (l == n) return(nomatch)
      l <- n
    } 
  }
}

`%BinIn%` <- function(x, table){
  BinMatch(x, table, nomatch = 0L) > 0L
}

# debug(BinMatch)
# print(BinMatch(c(-0.5, 3, 7.5, 12.5), (1:10)))
# print(c(-0.5, 3, 7.5, 12.5) %BinIn% (1:10))
# (function(){
#   require(microbenchmark)
#   for (i in 1:7){
#     stuff <- (1:(10^i))
#     print(paste0('1e',i))
#     print(microbenchmark(
#       3 %in% stuff,
#       3 %BinIn% stuff,
#       unit = 'us'))
#   }
# })()
