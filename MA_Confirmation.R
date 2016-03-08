##MA break-out to filter out false Turtle break out
require(quantmod)

#use potential from Turtle
potential <- as.vector(as.matrix(potential[,1]))
double_break_out <- rep(0,length.out=length(potential)) 
order_size <- rep(0,length.out=length(potential)) 
last_price <- rep(0,length.out=length(potential)) 
start_date <- "2015-09-01"

for(code in potential){
  symbol <- "" ##Initiate
  symbol <- paste(sprintf("%06d", code),"SS",sep=".")
  tryCatch({ getSymbols(symbol,from=start_date)
    ma100 <- SMA(Cl(get(symbol)),n=100) #100 days simple moving average
    ma10 <- SMA(Cl(get(symbol)),n=10) #10 days simple moving average
    if(last(ma10) > last(ma100))
    {double_break_out[which(potential == code)] <- 1
     order_size[which(potential == code)] <- 500/last(ATR(get(symbol))$atr)
     last_price[which(potential == code)] <- last(Cl(get(symbol)))
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

high_prob <- data.frame(cbind(potential,double_break_out,order_size,last_price))
##Get all the tickers of stocks that have Double Break_Out
high_prob <- high_prob[which(high_prob["double_break_out"]==1),]
