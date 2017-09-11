Problem42 <- function(){

  file <- 'C:/ProjectEuler/p042_words.txt'
  words <- transpose(fread(input = file, header = F))
  setnames(words, 'word')

  words[,
        value := sum(
          unlist(
            lapply(
              X = unlist(strsplit(word, split = '')),
              FUN = function(x){which(x == LETTERS)}
            )
          )
        ),
        word]
  
  maxvalue = max(words[,value])
  maxn = 2 * as.integer(maxvalue)

  triangles <- data.table(n = 1:maxn)
  triangles[, number := n*(n+1)/2]

  return(words[value %in% triangles[, number], .N])

}
