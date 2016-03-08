
# c(19,132,146,336,338,362,397,423,425,489,
#   507,508,547,706,811,854,891,975,988,997,999,1005,1069,1116,
#   1118,1186,1188,1567,1678,1798,1958,3123,3799,3968)


###Detect all tickers that have corresponding stocks in Shanghai Market
require(quantmod)

stocks_code <- seq(600001,603999,by = 1)
real_stocks <- rep(0,length.out=length(stocks_code))
start_date <- "2016-03-06"

for(code in stocks_code){
  if(code %% 1000 == 0) {print ("One Chapter")} #monitor progress
  symbol <- "" ##Initiate
  symbol = paste(sprintf("%06d", code),"SS",sep=".")
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
