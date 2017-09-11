Problem9 <- function(){
  temp <- data.table(expand.grid(a=1:1000,b=1:1000))
  temp <- temp[a < b]
  temp[, c := (1000 - b - a)]
  temp <- temp[(b < c) & (a^2 + b^2 == c^2), p := (a*b*c)]
  return(temp[, max(p, na.rm = T)])
}
