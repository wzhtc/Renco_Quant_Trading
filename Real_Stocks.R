# setwd("/Users/renco/GitHub/Renco_Quant_Trading")
# load("A_list.RData")

###Detect all tickers that have corresponding stocks in Shanghai Market
require(quantmod)

#stocks_code <- sprintf("%06d", seq(000001,000010,by = 1))
stocks_code <- seq(000001,000999)
real_stocks <- rep(0,length.out=length(stocks_code))
start_date <- "2016-03-06"

for(code in stocks_code){
  if(code %% 1000 == 0) {print ("One Chapter")} #monitor progress
  symbol <- "" ##Initiate
  symbol = paste(sprintf("%06d", code),
                 ifelse(code <= 1000, "SZ","SS")
                 ,sep=".") #"SS"
  #getSymbols(symbol,from=start_date) #Fecthing data
  tryCatch({getSymbols(symbol,from=start_date)
    real_stocks[which(stocks_code == code)] <- 1},  
  warning=function(msg) {
    print(paste("Caught warning message:", msg))
  },
  error=function(msg) {
    print(paste("Caught fatal message:", msg))
    return(NA)
  }
  )
}


SZ_list <-data.frame(stocks_code,real_stocks)
SZ_list <- SZ_list[which(SZ_list["real_stocks"]==1),]
SZ_list <- SZ_list["stocks_code"]


##Got Shenzhen Market
save(SZ_list,file="SZ_list.RData")

