rm(list = ls())

source('./DefineFunctions.R')
source('./DefineProblems.R')

results <- (function(){

  require(data.table)
  
  results <- data.table()
  
  for (x in 1L:59L){
    print(x)
    time <- system.time(
      eval(parse(text = paste0('ans <- Problems$Problem',x,'()'))))
    results <- rbind(
      results,
      data.table(
        Problem = x,
        Answer = ans,
        Time = time['elapsed']
      )
    )
  }
  
  return(results)
})()

# fwrite(x = results, file = 'results.csv', showProgress = T)
