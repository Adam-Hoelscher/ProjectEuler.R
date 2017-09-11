IsPalindrome <- function(x) {
  
  IsPalindrome1<-function(x){
    paste(rev(unlist(strsplit(x = as.character(x),split=''))),collapse='')==as.character(x)
  }
  
  unlist(lapply(X = x, FUN = IsPalindrome1))
  
}
