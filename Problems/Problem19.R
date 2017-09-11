Problem19 <- function() {
  library(data.table)
  temp <- data.table(AllDates=seq.Date(from = as.Date('1901-01-01'),
                                        to = as.Date('2000-12-31'),
                                        by = '1 day'))
  temp[,Dayofmonth:=mday(AllDates)]
  temp[,Dayofweek:=wday(AllDates)]
  return(temp[Dayofweek==1 & Dayofmonth==1,length(AllDates)])
}
