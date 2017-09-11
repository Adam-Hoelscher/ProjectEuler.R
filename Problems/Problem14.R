Problem14 <- function(to = 1e6){
  
  # replaced my original function with a version that uses an environment
  # object for the hashtable functionality
  
  Collatz <- function(x){
    if (x%%2) {
      return(3*x+1)
    } else {
      return(x/2)
    }
  }
  
  lengths <- new.env()
  
  assign(x = '1', value = 1L, envir = lengths)
  
  callcount <- 0L
  
  FindLength <- function(x){
    
    callcount <<- (callcount + 1L)
    
    # print(paste(callcount, x))
    chain <- mget(
      x = as.character(x),
      envir = lengths,
      ifnotfound = NA_integer_)[[1]]
    
    if (!is.na(chain)) return (chain)
    
    NextColl <- Collatz(x)
    
    len <- 1L + FindLength(NextColl)
    
    assign(
      x = as.character(x),
      value = len,
      envir = lengths)
    
    return(len)
  }
  
  temp <- sapply(
    FUN = FindLength,
    X = seq.int(
      from = 1,
      to = to,
      by = 2)
  )
  
  # print(callcount)
  return(2 * which.max(temp) - 1)
}

# attempted implementation using package "hashmap". turned out slower than
# using an enivronment
# Problem14 <- function(to = 1e6){
#   
#   require(hashmap)
# 
#   Collatz <- function(x){
#     if (x%%2) {
#       return(3*x+1)
#     } else {
#       return(x/2)
#     }
#   }
#   
#   lengths <- hashmap(1, 1L)
#   # by assigning names here we can avoid a lot of overhead from `$` extraction
#   find <- lengths$find
#   insert <- lengths$insert
#   
#   callcount <- 0L
#   
#   FindLength <- function(x){
#     
#     callcount <<- (callcount + 1L)
#     
#     # print(paste(callcount, x))
#     chain <- lengths$find(x)
#     
#     if (!is.na(chain)) return (chain)
#     
#     NextColl <- Collatz(x)
#     len <- 1L + FindLength(NextColl)
#     lengths$insert(x, len)
#     
#     return(len)
#   }
#   
#   temp <- sapply(
#     FUN = FindLength,
#     X = seq.int(
#       from = 1,
#       to = to,
#       by = 2)
#   )
#   
#   # print(callcount)
#   return(2 * which.max(temp) - 1)
# }

# Problem14 <- function(){
#   
#   require(data.table)
#   
#   NextCollatz <- function(x){
#     F1 <- function(x){
#       if(x%%2 == 0){
#         return(x/2)
#       } else {
#         return(3*x+1)
#       }
#     }
#     return(as.double(sapply(X = x, FUN = F1)))
#   }
#   
#   Coll <- data.table(x=as.double(1:1e6))
#   Coll[,n:=as.double(x)]
#   
#   while (Coll[, .N] > 1){
#     # print(Coll[, .N])
#     Coll[, n := NextCollatz(n)]
#     Coll <- Coll[n != 1]
#   }
#   
#   return(Coll[, x])
# }

# debug(Problem14)
# Rprof(interval = .001)
# print(system.time(print(Problem14(1e5))))
# Rprof(NULL)
# print(summaryRprof())
