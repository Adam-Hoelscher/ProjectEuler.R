Problem4<-function(){
  require(data.table)
  DT<-data.table(expand.grid(a=100:999,b=100:999))
  DT<-DT[a<=b]
  DT[,prod:=a*b]
  DT[,palindrome:=Functions$IsPalindrome(prod)]
  return(DT[palindrome==T,max(prod)])
}
