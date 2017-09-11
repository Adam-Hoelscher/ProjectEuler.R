Functions <- (function(){
  
  sourceFiles <- dir(
    path = './Functions',
    pattern = '.R',
    full.names = T)
  
  for (x in sourceFiles){
    # print(x)
    source(file = x, local = T)
  }

  rm(sourceFiles)  
  rm(x)

  return(as.list(environment()))
  
})()
