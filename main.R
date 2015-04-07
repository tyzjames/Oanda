source("getFunc.R")
source("misc.R")
source("helper.R")


apiKey<-"02c034fba14a63fcd5888ee9402d13dd-32f6fedc66beea46154f09ebef128124"

df.gold <- getHistoricalData(apiKey, "XAU_USD", "M10", "205")
df.silver <- getHistoricalData(apiKey, "XAG_USD", "M10", "205")

#Get midpoint
df.gold.mid <- getMidpoint(df.gold)
df.silver.mid <- getMidpoint(df.silver)

#Parameters
p.x<-100
st.dev<-2
cor.rng<-1
tsize<<-c(10,5)
mult<<-c(10,1000)  


df.Agold<-processData(df.gold.mid, p.x,  mult[1], tsize[1])
df.Asilver<-processData(df.silver.mid, p.x, mult[2], tsize[2])
df.Bmerged<-mergeData(df.Agold, df.Asilver,p.x)
df.Cprocessed<-processSignals(df.Bmerged, p.x, st.dev, tsize, 1,1)
