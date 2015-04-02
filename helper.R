library(caTools)
library(TTR)
library(gtools)

#Read CSV data
readData<-function(inFile){
  tempFile<-tryCatch({
    return(inFile[, c("datetime","open","high","low","close")])
    
  },
  error=function(cond){
    message("Data column headers do not match [datetime],[open],[high],[low],[close]")
    message("Reading data as [datetime],[open],[high],[low],[close]")
    return(inFile[, c(1:5)])
  },
  finally={

  })
}

#Process the raw csv data
processData <- function(inFile, p.x, multiplier, tickSize){
  
  df.temp<-data.frame(readData(inFile))
  
  #Set parameter
  colnames(df.temp) <- c("datetime","open","high","low","close")  
  df.temp[,c("open","high","low","close")]<-df.temp[,c("open","high","low","close")]*multiplier
  
  #Get trade direction
  #   df.temp$upDown <- (df.temp$open - df.temp$close)*-1/abs((df.temp$open - df.temp$close))
  
  #Get next minute data 
  #   df.temp$oneDayForward <- c(df.temp$upDown[-1],NA)
  
  #Get ATR
  df.temp$ATR <- ATR(df.temp[, c("high","low","close")], n = p.x)[, "atr"]*tickSize     
  df.temp$ATR[is.nan(df.temp$ATR)] = 0   #Replace NaN with 0
  
  #Get previous day prices
  df.temp$pOpen <- c(0, df.temp$open[-nrow(df.temp)])
  df.temp$pClose <- c(0, df.temp$close[-nrow(df.temp)])
  
  #Get highest high and lowerst low over previous x period and calculate relative true value
  df.temp$hHigh <- runmax(df.temp$high, p.x, align="right", endrule="NA") 
  df.temp$lLow <- runmin(df.temp$low, p.x, align="right", endrule="NA")
  df.temp$hHighlLowDiff <- df.temp$hHigh - df.temp$lLow
  df.temp$relCls <- (df.temp$close - df.temp$lLow)/df.temp$hHighlLowDiff
  
  #Rolling mean and SD for zScore
  df.temp$mean<-runmean(df.temp$open, p.x, alg=c("C"),endrule=c("mean"),align = c("right"))
  
  df.temp$sd<- runsd(df.temp$open, p.x, endrule=c("sd"),align = c("right"))
  
  df.temp$zScore<-(df.temp$open-df.temp$mean)/df.temp$sd
  
  vrb<-c("datetime","open","pOpen","close","pClose","relCls","ATR", "zScore")
  return (df.temp[,vrb])
}

#Merging data from two data frames
mergeData<-function(df.aa, df.bb, p.x){
  df.x<-merge(data.frame(df.aa[1:nrow(df.aa),]),data.frame(df.bb[1:nrow(df.bb),]), by="datetime")
  return (df.x)
}

#To process signals to enter/exit trade
processSignals<-function(df.merged, p.x, stdev, cor.rng, signal.type, ts, x.size, y.size, updateProgress=NULL){
  
  df.x<-df.merged
  
	df.x$s1s2<- round(df.x$relCls.x - df.x$relCls.y,3) #S1-S2 if positive correlation
	df.x$zScore<-round(df.x$zScore.x-df.x$zScore.y,3)

  #Get Exit signals
  df.x$exit<-ifelse(sign(df.x$s1s2)!=c(NA,sign(df.x$s1s2)[-nrow(df.x)]),1,0)
  df.x$fExit<-c(0,df.x$exit[-nrow(df.x)])
  
  #s1s2 BBand
  df.x$s1s2.bband<-BBands(HLC=df.x$s1s2, n = p.x, sd=stdev)
  
  #Add BBands for zScore. Execute when outside of BBand.
  df.x$z.bband<-BBands(HLC=df.x$zScore, n= p.x, sd=stdev) 
  
  #Get Primary Entry signals
  df.x$s1s2.signal<-ifelse(df.x$s1s2<=df.x$s1s2.bband[,"dn"], 1, ifelse(df.x$s1s2>=df.x$s1s2.bband[,"up"], 2, 0))
  
  #Compile all signals
  if ("zScore BBand" %in% signal.type) {
    df.x$sig.z <- ifelse(df.x$zScore<=df.x$z.bband[,"dn"] | df.x$zScore>= df.x$z.bband[,"up"], T, F)  
    df.x$sig.z[is.na(df.x$z.signal)] <- F
  } else {
    df.x$sig.z <- T
  }
  
  if ("s1s2 BBand" %in% signal.type) {
    df.x$sig.s1s2.long <- ifelse(df.x$s1s2.signal==1,T,F)
    df.x$sig.s1s2.sht <- ifelse(df.x$s1s2.signal==2,T,F)
  } else {
    df.x$sig.s1s2.long <- ifelse(df.x$s1s2>=0,T,F)
    df.x$sig.s1s2.sht <- ifelse(df.x$s1s2<0,T,F)
  }
  
  df.x$sig.final<-df.x$sig.z
  
  df.x$entry <- ifelse(df.x$sig.final & df.x$sig.s1s2.long, 1, ifelse(df.x$sig.final & df.x$sig.s1s2.sht, 2, 0))
  df.x$fEntry <- c(0,df.x$entry[-nrow(df.x)]) #forward signal
  #   
  #   #ATR Ratio
  #   #   df.x$ATR.ratio<-round(((df.x$ATR.y)/(df.x$ATR.x))*x.size) #ATR.y/ATR.x
  #   # df.x$ATR.ratio<-round(((df.x$ATR.x)/(df.x$ATR.y))*x.size) #ATR.x/ATR.y
  df.x$ATR.ratio<-y.size #ATR Ratio = 1
  
  vrb <- c("datetime","open.x","pOpen.x","close.x","pClose.x","open.y","pOpen.y","close.y","pClose.y"
           ,"s1s2","s1s2.signal","zScore","z.signal","signal","fSignal","exit","fExit","ATR.ratio","s1s2.slope",
           "z.bband.lower","z.bband.upper","z.bband.ma")
  return (df.x[(p.x*2):nrow(df.x),])
}

backTest<- function(df.ab, ts, x.size, updateProgress=NULL){
  df.x<-df.ab
  
  rowCount<-nrow(df.x)
  
  print("Backtesting")
  #Today's close minus today's open to calculate profit for Enter period
  df.x$clOp.x<-df.x$close.x-df.x$open.x
  df.x$clOp.y<-df.x$close.y-df.x$open.y
  
  #Today's close minus ytd close to calculate profit for holding period
  df.x$clPcl.x<-df.x$close.x-df.x$pClose.x
  df.x$clPcl.y<-df.x$close.y-df.x$pClose.y
  
  #Today's open minus ytd close to calculate profit for exit period
  df.x$opPcl.x<-df.x$open.x-df.x$pClose.x
  df.x$opPcl.y<-df.x$open.y-df.x$pClose.y
  
  df.x$pos.x<-0
  df.x$pos.y<-0
  df.x$size.x<-0
  df.x$size.y<-0
  df.x$price.x<-0
  df.x$price.y<-0
  df.x$pl.x<-0
  df.x$pl.y<-0
  
df.x$pos.x[df.x$fEntry==1]<-   1
df.x$pos.y[df.x$fEntry==1]<-   -1
df.x$pos.x[df.x$fEntry==2]<-   -1
df.x$pos.y[df.x$fEntry==2]<-   1

  df.x$size.x[df.x$entry!=0]<-x.size
  df.x$size.y[df.x$entry!=0]<-df.x$ATR.ratio[df.x$entry!=0]
  df.x$price.x[df.x$fEntry!=0]<-df.x$open.x[df.x$fEntry!=0]
  df.x$price.y[df.x$fEntry!=0]<-df.x$open.y[df.x$fEntry!=0]
  
  df.x$inTrade<-0
  inTrade<-F
  startRow<-0
  endRow<-0
  
  for (i in 2:rowCount){
    if (!inTrade) {
      if (df.x$fEntry[i]!=0){
        inTrade<-T
        startRow<-i
      }
    } else {
      if (df.x$fExit[i]==1){
        inTrade<-F
        endRow<-i
        df.x$inTrade[startRow:endRow]<-1 #Hold trade
        df.x$inTrade[startRow]<-2 #Enter
        df.x$inTrade[endRow]<-3 #Exit
        df.x$pos.x[startRow:endRow]<-df.x$pos.x[startRow]
        df.x$pos.y[startRow:endRow]<-df.x$pos.y[startRow]
        df.x$price.x[startRow:endRow]<-df.x$price.x[startRow]
        df.x$price.y[startRow:endRow]<-df.x$price.y[startRow]
        df.x$size.x[startRow:endRow]<-df.x$size.x[startRow-1]
        df.x$size.y[startRow:endRow]<-df.x$size.y[startRow-1]
        
        startRow<-0
        endRow<-0
      }
    }
    
    if (is.function(updateProgress) & i%%(rowCount%/%20)==0) {
      text <- paste("Backtesting:", paste(as.character(round((i/rowCount)*100)),"%",sep=""))
      updateProgress(detail = text)
    }
  }
  
  enterTrade<- df.x$inTrade==2
  holdTrade<-df.x$inTrade==1
  exitTrade<-df.x$inTrade==3
 
  df.x$pl.x[enterTrade]<-df.x$pos.x[enterTrade]*df.x$size.x[enterTrade]*df.x$clOp.x[enterTrade]*ts[1]
  df.x$pl.y[enterTrade]<-df.x$pos.y[enterTrade]*df.x$size.y[enterTrade]*df.x$clOp.y[enterTrade]*ts[2]
  
  df.x$pl.x[exitTrade]<-df.x$pos.x[exitTrade]*df.x$size.x[exitTrade]*df.x$opPcl.x[exitTrade]*ts[1]
  df.x$pl.y[exitTrade]<-df.x$pos.y[exitTrade]*df.x$size.y[exitTrade]*df.x$opPcl.y[exitTrade]*ts[2]
  
  df.x$pl.x[holdTrade]<-df.x$pos.x[holdTrade]*df.x$size.x[holdTrade]*df.x$clPcl.x[holdTrade]*ts[1]
  df.x$pl.y[holdTrade]<-df.x$pos.y[holdTrade]*df.x$size.y[holdTrade]*df.x$clPcl.y[holdTrade]*ts[2]
  
  df.x$daily.pl<-round(df.x$pl.x + df.x$pl.y)
  df.x$total.pl<-round(cumsum(df.x$daily.pl))
  
  return (df.x)
}

#Return table that is organized for download
processDownloadData<-function(input){
  df.eBTemp<-input
  df.eBTemp <- df.eBTx[,c("datetime","open.x","close.x","open.y","close.y", "s1s2","zScore", "cor", "pos.x", "size.x", "pos.y","size.y","price.x","price.y","pl.x","pl.y","daily.pl","total.pl","inTrade")]   
  df.eBTemp$pos.x.str<-ifelse(df.eBTemp$pos.x==-1,"Short",ifelse(df.eBTemp$pos.x==1,"Long",""))
  df.eBTemp$pos.y.str<-ifelse(df.eBTemp$pos.y==-1,"Short",ifelse(df.eBTemp$pos.y==1,"Long",""))
  df.eBTemp$inTrade.str<-ifelse(df.eBTemp$inTrade==2,"Enter",ifelse(df.eBTemp$inTrade==3,"Exit",ifelse(df.eBTemp$inTrade==1,"Hold","")))
  df.eBTemp<-df.eBTemp[c("datetime","open.x","close.x","open.y","close.y", "s1s2","zScore", "cor", "inTrade.str","pos.x.str", "size.x", "pos.y.str","size.y","price.x","price.y","pl.x","pl.y","daily.pl","total.pl")]
  colnames(df.eBTemp)<-c("Date and Time", "O1","C1", "O2","C2", "S1S2","zScore", "Cor", "Signal","Pos1","Size1","Pos2","Size2","Price1","Price2","Profit1","Profit2","P/L","Total P/L")
  
  return(df.eBTemp)
}

#Get slope from 0 to threshold
#We want to exclude the steep slopes
getSlope<-function(df.temp,thres, updateProgess=NULL){
  df.temp$s1s2.slope <- 0
  inTrade<-F
  hasExit<-F
  rowCount<-nrow(df.temp)
  print("Getting slope")
  
  for (i in 1:rowCount){
    if (df.temp$exit[i]==0){
      hasExit<-T
    }
    
    if (df.temp$s1s2.signal[i] != 0 & hasExit==T){
      inTrade<-T
      j = i-1
      while (df.temp$exit[j] != 0){
        j<-j-1
      } 
      df.temp$s1s2.slope[i]<-thres/(i-j)
    } 
    
    if (is.function(updateProgress) & i%%(rowCount%/%20)==0) {
      text <- paste("Getting Slope:", paste(as.character(round((i/rowCount)*100)),"%",sep=""))
      updateProgress(detail = text)
    }
    
  }
  
  return (df.temp$s1s2.slope)
}



