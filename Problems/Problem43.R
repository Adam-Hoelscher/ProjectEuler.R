Problem43 <- function(){

  Div1  <- data.table(n = 1:999)
  Div1[, n := sprintf('%03d', n)]
  Div1[, `:=`(d1 = substr(n, 1, 1),
              d2 = substr(n, 2, 2),
              d3 = substr(n, 3, 3),
              n  = NULL
  )]
  
  Div2  <- data.table(n = which((1:999 %% 2)  == 0))
  Div2[, n := sprintf('%03d', n)]
  Div2[, `:=`(d2 = substr(n, 1, 1),
              d3 = substr(n, 2, 2),
              d4 = substr(n, 3, 3),
              n  = NULL
  )]
  
  Div3  <- data.table(n = which((1:999 %% 3)  == 0))
  Div3[, n := sprintf('%03d', n)]
  Div3[, `:=`(d3 = substr(n, 1, 1),
              d4 = substr(n, 2, 2),
              d5 = substr(n, 3, 3),
              n  = NULL
  )]
  
  Div5  <- data.table(n = which((1:999 %% 5)  == 0))
  Div5[, n := sprintf('%03d', n)]
  Div5[, `:=`(d4 = substr(n, 1, 1),
              d5 = substr(n, 2, 2),
              d6 = substr(n, 3, 3),
              n  = NULL
  )]
  
  Div7  <- data.table(n = which((1:999 %% 7)  == 0))
  Div7[, n := sprintf('%03d', n)]
  Div7[, `:=`(d5 = substr(n, 1, 1),
              d6 = substr(n, 2, 2),
              d7 = substr(n, 3, 3),
              n  = NULL
  )]
  
  Div11 <- data.table(n = which((1:999 %% 11) == 0))
  Div11[, n := sprintf('%03d', n)]
  Div11[,`:=`(d6 = substr(n, 1, 1),
              d7 = substr(n, 2, 2),
              d8 = substr(n, 3, 3),
              n  = NULL
  )]
  
  Div13 <- data.table(n = which((1:999 %% 13) == 0))
  Div13[, n := sprintf('%03d', n)]
  Div13[,`:=`(d7 = substr(n, 1, 1),
              d8 = substr(n, 2, 2),
              d9 = substr(n, 3, 3),
              n  = NULL
  )]
  
  Div17 <- data.table(n = which((1:999 %% 17) == 0))
  Div17[, n := sprintf('%03d', n)]
  Div17[,`:=`(d8 = substr(n, 1, 1),
              d9 = substr(n, 2, 2),
              d0 = substr(n, 3, 3),
              n  = NULL
  )]
  
  
  temp <- merge(Div1, Div2,  c('d2', 'd3'), allow.cartesian = T)
  temp <- merge(temp, Div3,  c('d3', 'd4'), allow.cartesian = T)
  temp <- merge(temp, Div5,  c('d4', 'd5'), allow.cartesian = T)
  temp <- merge(temp, Div7,  c('d5', 'd6'), allow.cartesian = T)
  temp <- merge(temp, Div11, c('d6', 'd7'), allow.cartesian = T)
  temp <- merge(temp, Div13, c('d7', 'd8'), allow.cartesian = T)
  temp <- merge(temp, Div17, c('d8', 'd9'), allow.cartesian = T)

  temp[, n := paste0(d1, d2, d3, d4, d5, d6, d7, d8, d9, d0)]
  temp[, check := Functions$IsPandigital(n, n = 0:9), n]
  temp <- temp[which(check)]

  return(temp[,sum(as.numeric(n))])
}
