Problem1<-function(){
  return (sum((1:999)[1:999 %% 3 == 0 | 1:999 %% 5 == 0]))
}
