Problem35 <- function() {
  
  power <- 6
  
  rotate <- function(x) {
    x <- as.character(x)
    x <- strsplit(x = x, split = '')
    x <- unlist(x)
    x <- c(x[-1],x[1])
    x <- as.integer(paste(x, collapse = ''))
    return(x)
  }

  containseven <- function(x) {
    x <- as.character(x)
    x <- unlist(strsplit(x = x, split = ''))
    if (length(setdiff(x = c(0,2,4,6,8),y = x)) != 5) {
      return(T)
    } else {
      return(F)
    }
  }

  test <- data.table(orig = 1:((10 ^ power) - 1))
  test <- test[
    j  = `:=`(even = containseven(orig)),
    by = orig
    ]
  test <- test[
    i = !(even == T) | (orig == 2)][
      j = `:=`(even = NULL)
      ]
  test[, curr := orig]
  
  for (x in 1:power) {
    test <- test[Functions$IsPrime(curr)]
#     test <- test[prime == T]
    test[, curr := rotate(curr), orig]
  }
  
#   temp <- test[prime == T, length(orig)]
  return(dim(test)[1])

}

# debug(Problem35)
# print(Problem35())
