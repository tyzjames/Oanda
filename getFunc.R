library(rjson)
library(RCurl)
source("misc.R")

getHistoricalData<-function(apiKey, instrument, granularity, lookBack) {
  #Gets historical data from Oanda practice account
  
  #query <- 'https://api-fxpractice.oanda.com/v1/candles?instrument=XAU_SGD&count=10&granularity=M1'
  #auth <- c(Authorization='Bearer 02c034fba14a63fcd5888ee9402d13dd-32f6fedc66beea46154f09ebef128124')
  
  instru_param <- paste("instrument=",instrument, sep = "")
  gran_param <- paste("granularity=", granularity, sep = "")
  lb_param <- paste("count=", lookBack, sep = "")
  
  query <- 'https://api-fxpractice.oanda.com/v1/candles?'
  param <- paste(instru_param, gran_param, lb_param, sep="&")
  query <- paste(query, param, sep="")
  
  Bearer <- paste('Bearer', apiKey, sep=" ")
  auth <- c(Authorization=Bearer)
  
  getData <- getURL(query, httpheader = auth, ssl.verifypeer = F)
  
  jsonData<- fromJSON(getData)
  
  df.return<-data.frame(do.call("rbind",jsonData$candles[]))
  
  df.return$time<-convertOandaTimeToPOSIXlt(df.return$time)
  
  return(df.return)
}