###Turtle Trading Detector
require(quantmod)

#stocks_code <- seq(600001,603999,by = 1)
setwd("/Users/renco/GitHub/Renco_Quant_Trading")
load("A_list.RData")
stocks_code <- as.vector(as.matrix(A_list)) #for looping to saving time
break_out <- rep(0,length.out=length(stocks_code)) #for vector
# break_out <- rep(0,length.out=dim(stocks_code)[1]) #for data.frame
start_date <- "2016-01-01"

for(code in stocks_code){
  #if(code %% 1000 ==0) {print (code)} #monitor progress
  symbol <- "" ##Initiate
  symbol <- paste(sprintf("%06d", code),"SS",sep=".")
  #getSymbols(symbol,from=start_date) #Fecthing data
  tryCatch({getSymbols(symbol,from=start_date)
            #atr <- lag(ATR(get(symbol)))
            dc <- lag(DonchianChannel(cbind(Hi(get(symbol)), Lo(get(symbol)))))
            if(last(Cl(last(get(symbol)))) > last(dc)$high)
            {break_out[which(stocks_code == code)] <- 1
            } else {removeSymbols(symbol) #drop uninteresting data
            }
  },  
           warning=function(msg) {
             print(paste("Caught warning message:", msg))
           },
           error=function(msg) {
             print(paste("Caught fatal message:", msg))
             return(NA)
           }
  ) #tryCatch
} #for 

##Delete data that doesn't matter 

potential <- data.frame(cbind(stocks_code,break_out))
##Get all the tickers of stocks that have Donchian Break_Out
potential <- potential[which(potential["break_out"]==1),]

####Add code to save potential

save(potential,file=paste("potential",Sys.Date(),sep="_"))





