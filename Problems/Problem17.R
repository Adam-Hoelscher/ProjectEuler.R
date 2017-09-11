Problem17 <- function() {
  
  Teens <- function (n){
    switch(EXPR = (n - 9),
      'ten', 
      'eleven', 
      'twelve', 
      'thirteen', 
      'fourteen',
      'fifteen',
      'sixteen',
      'seventeen', 
      'eighteen',
      'nineteen' 
    )
  }
  
  Tens <- function(n) {
    switch(EXPR = (n - 1),
      'twenty',
      'thirty',
      'forty',
      'fifty',
      'sixty',
      'seventy', 
      'eighty',
      'ninety' 
    )
  }
  
  Ones <- function(n) {
    switch(EXPR = n,
      'one',
      'two',
      'three', 
      'four',
      'five',
      'six',
      'seven', 
      'eight',
      'nine' 
    )
  }

  Words <- function(n) {
    s <- ''
    if (n >= 1000) {
      s <- paste(s,Ones(n / 1000),'thousand')
    }
    n <- n %% 1000
    if (n >= 100) {
      s <- paste(s,Ones(n / 100),'hundred')
    }
    n <- n %% 100
    if (n != 0) {
      if (nchar(s) != 0){
        s <- paste(s,'and')
      }
      if ((n < 20) && (n > 9)){
        s <- paste(s,Teens(n))
      } else {
        s <- paste(s,Tens(n / 10))
        n <- n %% 10
        s <- paste(s,Ones(n))
      }
    }
    return (s)
  }

  temp <- paste(sapply(X = 1:1000, FUN = Words), collapse = '')
  temp <- gsub(pattern = ' ', replacement = '', temp)
  return(nchar(temp))
}

# debug(Problem17)
# print(Problem17())
