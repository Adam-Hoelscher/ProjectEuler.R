Problem53 <- function(){
  temp <- 0L
  for (n in 1:100){
    for (r in 1:n){
      if (choose(n,r)>1000000) {temp <- temp + 1L}
    }
  }
  return (temp)
}

# print(system.time(print(Problem53())))
