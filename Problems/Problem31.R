Problem31 <- function() {

  cache <- data.table(amountC = integer(0),
                      coinsC = integer(0),
                      countC = integer(0))

  CountCoins <- function(amount,coins) {
    coins <- unique(sort(coins, decreasing = T))
    checkCache <- cache[(amountC == amount) &
                          (coinsC == length(coins)),
                        countC]
    if (length(checkCache) == 1) {return (checkCache)}
    if (length(coins) == 1) {
      if (amount %% coins == 0) {
        temp <- 1
      } else {
        temp <- 0
      }
    } else {
      options <- data.table(n = 0:(as.integer(amount / coins[1])))
      options[,value:=(n*coins[1])]
      options[,remainder:=(amount-value)]
      options[,count:=unlist(lapply(X = remainder,
                                    FUN = CountCoins,
                                    coins = coins[-1]))]
      temp <- options[,sum(count)]

      cache <<- rbind(cache,
                      data.table(amountC = amount,
                                 coinsC=length(coins),
                                 countC=temp))

      return(temp)
    }
  }

  amount <- 200
  coins <- c(1,2,5,10,20,50,100,200)
  temp <- CountCoins(amount, coins)
  return(temp)

}

# debug(Problem31)
# print(system.time(print(Problem31())))
