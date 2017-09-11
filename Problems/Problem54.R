Problem54 <- function(){
  
  deals <- read.table(
    file = 'C:/ProjectEuler/p054_poker.txt',
    header = F,
    sep = ' ',
    as.is = T)
  
  # TODO: makes changes to use data.table
  # deals <- fread(
  #   input = 'C:/ProjectEuler/p054_poker.txt',
  #   header = F,
  #   sep = ' ')
  
  Hand <- function(cards){
    
    ranks <- match(
      x = substr(cards,1,1),
      table = c(1:9,'T','J','Q','K','A'))
    suits <- substr(cards,2,2)
    
    freq <- table(ranks)
    c2 <- sum(freq==2)
    c3 <- sum(freq==3)
    c4 <- sum(freq==4)
    r2 <- names(freq)[which.max(freq==2)]
    r3 <- names(freq)[which.max(freq==3)]
    r4 <- names(freq)[which.max(freq==4)]
    
    Is.Straight <-
      (all(max(ranks)  == min(ranks) + 4) &
         max(freq) == 1) ||
      all(sort(ranks) == c(2:5, 14))
    
    Is.Flush <-
      (length(unique(substr(cards,2,2))) == 1)
    
    if (Is.Straight && Is.Flush){
      val <- 9
      tie <- max(ranks)
    } else if (c4 == 1){
      val <- 8
      tie <- as.integer(r4)
    } else if ((c3 == 1) & (c2 == 1)){
      val <- 7
      tie <- as.integer(r3)
    } else if (Is.Flush){
      val <- 6
      tie <- max(ranks)
    } else if (Is.Straight){
      val <- 5
      tie <- max(ranks)
    } else if (c3 == 1){
      val <- 4
      tie <- as.integer(r3)
    } else if (c2 == 2){
      val <- 3
      tie <- as.integer(r2)
    } else if (c2 == 1){
      val <- 2
      tie <- as.integer(r2)
    } else {
      val <- 1
      tie <- max(ranks)
    }
    
    tie2 <- sort(x = ranks, decreasing = T)
    tie2 <- sprintf('%02d', tie2)
    tie2 <- paste0(tie2, collapse = '')
    
    temp <- (100*val + tie)
    temp <- (temp*1E10 + as.integer(tie2))
    return(temp)
    
  }
  
  # debug(Hand)
  
  for (x in 1L:nrow(deals)){
    deals[x,11] <- Hand(unlist(deals[x, 1:5 ]))
    deals[x,12] <- Hand(unlist(deals[x, 6:10]))
  }
  
  deals[,13] <- (deals[,11] >= deals[,12])
  
  # View(deals)
  # print(which(deals[,13]))
  return(sum(deals[,13]))
  
}

# debug(Problem54)
# print(system.time(print(Problem54())))
