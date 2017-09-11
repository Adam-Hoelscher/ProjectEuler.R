Problem22 <- function() {

  names <- scan(file = 'C:/ProjectEuler/p022_names.txt',
                na.strings = character(0),
                what = 'character',
                sep = ',',
                quiet = T,
                quote = '"')

  score <- function(name) {
    temp <- unlist(strsplit(x = name, split = ''))
    temp <- unlist(lapply(X = temp, FUN = utf8ToInt))
    temp <- temp - utf8ToInt('A') + 1
    return(sum(temp))
  }

  temp <- data.table(names = sort(names))
  temp[,scores:=unlist(lapply(X = names,FUN = score))]
  temp[,wscore:=scores*1:dim(temp)[1]]
  return(temp[,sum(wscore)])
}

# debug(Problem22)
# print(Problem22())
