convertOandaTimeToPOSIXlt <- function (df.time) {
  
  df.temp<-as.POSIXlt(paste(substr(df.time, 1, 10), substr(df.time, 12, 19), sep=" "))
  
  return (df.temp)
}

getMidpoint <- function (df.x) {
  
  
  return (data.frame(Time = df.x$time, Open = (as.numeric(df.x$openBid) + as.numeric(df.x$openAsk))/2, 
                                               High = (as.numeric(df.x$highBid) + as.numeric(df.x$highAsk))/2,
                                                       Low = (as.numeric(df.x$lowBid) + as.numeric(df.x$lowAsk))/2, 
                                                              Close = (as.numeric(df.x$closeBid) + as.numeric(df.x$closeAsk))/2))
  
}