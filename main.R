source("getFunc.R")
source("misc.R")
source("helper.R")


apiKey<-"02c034fba14a63fcd5888ee9402d13dd-32f6fedc66beea46154f09ebef128124"

df.gold <- getHistoricalData(apiKey, "XAU_USD", "M1", "5000")
df.silver <- getHistoricalData(apiKey, "XAG_USD", "M1", "5000")

df.gold.mid <- getMidpoint(df.gold)
df.silver.mid <- getMidpoint(df.silver)

p.x<-1000
st.dev<-2
cor.rng<-1
tsize<<-c(10,5)
mult<<-c(10,1000)  


df.Agold<-processData(df.gold.mid, p.x,  mult[1], tsize[1])
df.Asilver<-processData(df.silver.mid, p.x, mult[2], tsize[2])

df.Bmerged<-mergeData(df.Agold, df.Asilver,p.x)

df.cprocessed<-processSignals(df.Bmerged, p.x, st.dev, c(-1,1), c("s1S2 BBand","zScore BBand"), tsize, 1,1)

df.dBT <- backTest(df.cprocessed, tsize, 1)

plot(df.dBT$total.pl, type='l')
