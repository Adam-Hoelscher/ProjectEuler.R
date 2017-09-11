Problem49 <- function(){
  
  require(data.table)
  
  temp <- data.table(number = 1000:9999)

  temp[,
       base := paste0(
         sort(unlist(strsplit(as.character(number), split = ''))),
         collapse = ''),
       number]
  
  temp <- temp[Functions$IsPrime(number)]
  
  temp <- merge(x  = temp,
                y  = temp,
                by = 'base',
                allow.cartesian = T)
  
  temp <- temp[number.x < number.y]
  
  temp <- temp[3330 == (number.y - number.x) & base != '1478']

  checkbase <- temp[,.N,(base)][N>1][, base]
  temp <- temp[base == checkbase]
  
  return(paste0(temp[1, number.x],
                temp[1, number.y],
                temp[2, number.y],
                collapse = ''))
  
}

# print(system.time(print(Problem49())))
