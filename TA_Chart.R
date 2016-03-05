##Technical Analysis of Renco 

code = "SSEC"
Is_Indice = 1
location = c("SS","SZ")
loc_code = 1 
start_date = "2015-06-01"



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
addADX(n=14,maType="EMA",wilder=TRUE)
addMACD()
addBBands()
addSAR()
##Donchian Chanles
dc <- lag(DonchianChannel(cbind(Hi(get(symbol)), Lo(get(symbol)))))
addTA(dc$low, on=1, col='green4')
addTA(dc$high, on=1, col='green4')
