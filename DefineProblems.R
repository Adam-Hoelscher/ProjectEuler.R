Problems <- (function(){
  
  sourceFiles <- dir(
    path = './Problems',
    pattern = '.R',
    full.names = T)
  
  for (x in sourceFiles){
    # print(x)
    source(file = x, local = T)
  }

  return(as.list(environment()))
  
})()