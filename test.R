library(rjson)
library(RCurl)
library(fImport)
source("getFunc.R")
source("misc.R")

#query <- 'https://api-fxpractice.oanda.com/v1/accounts'

getHistoricalData<-function(apiKey, instrument, granularity, lookBack) {
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
  
  return(data.frame(do.call("rbind",jsonData$candles[])))
}

apiKey<-"02c034fba14a63fcd5888ee9402d13dd-32f6fedc66beea46154f09ebef128124"

df.gold <- getHistoricalData(apiKey, "XAU_USD", "H1", "50")
df.silver <- getHistoricalData(apiKey, "XAG_USD", "H1", "50")

df.temp.date<- substr(df.gold$time, 1, 10)
df.temp.time<- substr(df.gold$time, 12, 19)

convertOandaTimeToPOSIXct(df.gold$time)
