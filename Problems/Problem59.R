Problem59 <- function(verbose = F){

  code <- function(s) {strtoi(charToRaw(s), 16L)}
  char <- function(n) {rawToChar(as.raw(n))}
  bitsToInt <- function(x) {
    sum(2^(which(unlist(strsplit(as.character(as.integer(x)), "")) == 1)-1))
  }

  ctext <- readLines(con <- file('C:/ProjectEuler/p059_cipher.txt'))
  close(con)
  ctext <- unlist(strsplit(x = ctext, split = ','))
  
  lot1 <- ctext[seq.int(1, 1201, 3)]
  lot2 <- ctext[seq.int(2, 1199, 3)]
  lot3 <- ctext[seq.int(3, 1200, 3)]
  
  testKey <- function(key, lot, return = 'score'){
    
    key <- rep(x = key, times = (length(lot)/length(key))+1)
    key <- key[1:(length(lot))]
    key <- unlist(lapply(X = key, FUN = code))
    key <- matrix(data = intToBits(key), ncol = 32, byrow = T)
    
    cbits <- unlist(lapply(X = lot, FUN = intToBits))
    cbits <- matrix(data = cbits, ncol = 32, byrow = T)
    
    pbits <- xor(cbits, key)
    
    ptext <- apply(
      X = pbits,
      MARGIN = 1,
      FUN = function(x){char(bitsToInt(x))})

    score <- sum(ptext == ' ')
    ASCII <- sum(apply(X = pbits, MARGIN = 1, FUN = function(x){bitsToInt(x)}))
    ptext <- paste(ptext, collapse = '')
    
    if (return == 'ASCII'){
      return (ASCII)
    } else if (return == 'ptext'){
      return (ptext)
    } else {
      return (score)
    }
    
  }

  k1 <- unlist(lapply(X = letters, FUN = testKey, lot = lot1))
  k2 <- unlist(lapply(X = letters, FUN = testKey, lot = lot2))
  k3 <- unlist(lapply(X = letters, FUN = testKey, lot = lot3))

  key <- unlist(
    lapply(X = list(k1, k2, k3),
           FUN = function(x){letters[which.max(x)]}))

  if (verbose){
    print(testKey(key = key, lot = ctext, return = 'ptext'))
    print(paste(key, collapse = ''))
  }
  return(testKey(key = key, lot = ctext, return = 'ASCII'))
  
}

# debug(Problem59)
# print(system.time(print(Problem59(T))))
