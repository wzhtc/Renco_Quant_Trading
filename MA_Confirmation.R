##MA break-out to filter out false Turtle break out
require(quantmod)

#use potential from Turtle
double_break_out <- rep(0,length.out=dim(potential)[1])
start_date <- "2015-12-01"

for(code in potential["stocks_code"]){
  symbol <- "" ##Initiate
  symbol <- paste(sprintf("%06d", code),"SS",sep=".")
  tryCatch({ getSymbols(symbol,from=start_date)
    ma100 <- SMA(get(symbol),n=100) #100 days simple moving average
    ma10 <- SMA(get(symbol),n=10) #10 days simple moving average
    if(last(ma10) > last(ma100))
    {break_out[which(stocks_code == code)] <- 1}
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

high_prob <- data.frame(cbind(potential["stocks_code"],double_break_out))
##Get all the tickers of stocks that have Double Break_Out
high_prob <- high_prob[which(high_prob["break_out"]==1),]
