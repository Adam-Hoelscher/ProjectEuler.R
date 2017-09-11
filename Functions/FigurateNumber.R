FigurateNumber <- function(n, s){
  ((n**2)*(s-2) - n*(s-4))/2
}

IsFigurate <- function(x, s = 0, n = 0, warn.trivial = T){
  if (s*n){
    if (warn.trivial){
      warning('provided both side and number')
    }
    return(x == FigurateNumber(s,n))
  } else if (s) {
    n <- ((8*(s-2)*x+(s-4)^2)^.5+(s-4))/(2*(s-2))
    return(n == as.integer(n))
  } else if (n) {
    
  }
}

# debug(IsFigurate)
# print(IsFigurate(x = 40755, s = 5))