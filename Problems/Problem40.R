Problem40 <- function(){

  decimals <- paste0(1:10E5, collapse = '')
  values <- data.table(power = 0:6)
  values[, place := as.integer(
    substr(x = decimals,
           start = (10 ^ power),
           stop  = (10 ^ power)
    )),
    power
    ]
  
  return(values[, prod(place)])    
}

# debug(Problem39)
# print(system.time(print(Problem40())))
