Problem18 <- function() {

  temp <- as.matrix(read.table(file = 'C:/ProjectEuler/p018_triangle.txt',
                            col.names = 1:15,
                            fill = T))

  for (r in 14:1) {
    for (c in r:1) {
      add <- max(c(temp[r+1,c], temp[r+1,c+1]))
      temp[r,c] <- (temp[r,c] + add)
    }
  }
  return(as.integer(temp[1,1]))
}

# debug(Problem18)
# print(Problem18())
