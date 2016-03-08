##Technical Analysis of Renco 

code = 600005
Is_Indice = 0
location = c("SS","SZ")
loc_code = 1 
start_date = "2016-01-01"



if(typeof(code) != "character"){ 
  #Indices and US stocks are characters
  symbol = paste(sprintf("%06d", code),location[loc_code],sep=".")
} else if(Is_Indice == 1){
  symbol = paste("^",code,sep="") 
} else {
  symbol = code
}


getSymbols(symbol,from=start_date) #Fecthing data

#####Chart
if(Is_Indice !=1){
  chartSeries(get(symbol),theme="white",name=symbol)
} else {
  chartSeries(get(code),theme="white",name=symbol)
}
###TA to use
#addADX(n=14,maType="EMA",wilder=TRUE)
addMACD()
addBBands()
addATR()
#addSAR()
##Donchian Chanles
Has_Donchian = 1
if(Has_Donchian == 1){
dc <- lag(DonchianChannel(cbind(Hi(get(symbol)), Lo(get(symbol)))))
addTA(dc$low, on=1, col='green4')
addTA(dc$high, on=1, col='green4')
}
##Moving Averages
##Note I haven't work on colors of MA yet 
Has_MA = 0
if(Has_MA == 1)
{
addSMA(n=5)
addSMA(n=10)
addSMA(n=20)
}