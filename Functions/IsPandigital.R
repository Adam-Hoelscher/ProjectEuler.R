IsPandigital <- function(..., n = 9) {
  
  if (length(n) == 1){n <- 1:n}
  stuff <- as.character(paste0(c(...), collapse = ''))
  stuff <- unlist(lapply(X = stuff, FUN = strsplit, split = ''), recursive = T)
  stuff <- paste0(sort(stuff), collapse = '')
  temp <- (stuff == paste(n, collapse = ''))
  
  return(temp)
}
