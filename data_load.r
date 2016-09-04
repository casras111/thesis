library(xlsx)
library(quantmod)

startDate <- "1996/1/1"
endDate   <- "2015/12/31"
period_filter <- paste0(substr(startDate,1,4),"::",substr(endDate,1,4))
options("getSymbols.warning4.0"=FALSE)

# Get stock data from Yahoo Finance, daily
mysymbols <- c("IBM","AAPL","IHT","USEG","LNN")
getSymbols(mysymbols,src="yahoo", auto.assign = T,from=startDate,to=endDate)
s1 <- Ad(get(mysymbols[1])) #keep only Adjusted.Close
Stocks <- s1
for (i in 2:length(mysymbols)) {
  Stocks <- merge.xts(Stocks,Ad(get(mysymbols[i])))
}
colnames(Stocks) <- mysymbols
Stocks <- Stocks[endpoints(Stocks,on="months"),] #only end of month prices

##### read LIBOR daily rates  ######
getSymbols("USD1MTD156N",src="FRED",auto.assign = T,from=startDate,to=endDate)
LIBOR <- na.locf(USD1MTD156N)
LIBOR <- LIBOR[endpoints(LIBOR,on="months"),] #keep only end of month
colnames(LIBOR) <- "LIBOR"
LIBOR <- LIBOR[period_filter]

##### read MSCI index monthly prices  ######
mpath <- file.path("../DataRaw","MSCI_ACWI.xls")
MSCI <- read.xlsx(file=mpath,sheetIndex=1,startRow = 7,endRow = 346)
MSCI <- xts(MSCI[,2],MSCI$Date)
colnames(MSCI) <- "MSCI"
MSCI <- MSCI[period_filter]

#dates for end of month do not match exactly, 1 and 3 day lag for stocks vs msci
#Fix by changing end of month dates in MSCI and LIBOR to match Stocks dates
#Introduces possible error of 1-3 days in 5 entries for the 20 years checked
#Error in 5/240 entries
which(is.na(merge(MSCI,LIBOR,Stocks)$MSCI))
which(index(MSCI)!=index(Stocks))
mismatches <- which(index(MSCI)!=index(Stocks))
index(MSCI)[mismatches]  <- index(Stocks)[mismatches]
index(LIBOR)[mismatches] <- index(Stocks)[mismatches]
which(is.na(merge(MSCI,LIBOR,Stocks)$MSCI))

indexcap <- 33500 #$33500 milliard MSCI ACWI
cap <- c(152.5,580.5,0.02197,0.00887,0.77376)
cap_pct <- cap/indexcap
names(cap_pct) <- mysymbols

#Save Rdata variables for calculate module
save(Stocks,file="../DataWork/Stocks.Rdata")
save(LIBOR,file="../DataWork/LIBOR.Rdata")
save(MSCI,file="../DataWork/MSCI.Rdata")
save(cap_pct,file="../DataWork/cap_pct.Rdata")
