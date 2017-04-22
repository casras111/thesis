library(xlsx)
library(quantmod)

hist_daily=F
startDate <- "1996/1/1"
endDate   <- "2015/12/31"
period_filter <- paste0(substr(startDate,1,4),"::",substr(endDate,1,4))
options("getSymbols.warning4.0"=FALSE)

# Get stock data from Yahoo Finance, daily
#mysymbols <- c("IBM","AAPL","IHT","USEG","LNN")
#read companies names from screening file with US stocks from before 1960
#daily vol greater than 0.1mm
allstocks <- read.csv("../DataRaw/screen_companies_no_otc.csv",sep=",",stringsAsFactors = F)
#mysymbols <- sample(allstocks[,2],10)
mysymbols <- allstocks[,2]
getSymbols(mysymbols,src="yahoo", auto.assign = T,from=startDate,to=endDate)
s1 <- Ad(get(mysymbols[1])) #keep only Adjusted.Close, get for string to var
Stocks <- s1
for (i in 2:length(mysymbols)) {
  Stocks <- merge.xts(Stocks,Ad(get(mysymbols[i])))
}
colnames(Stocks) <- mysymbols

#remove non-trade day after Sep 11 attack, lots of NAs
Stocks <- Stocks[index(Stocks)!="2001-09-12"]

#remove stocks with missing trading info
missing_vec <- sapply(Stocks,anyNA)
Stocks <- Stocks[,!missing_vec]
mysymbols <- mysymbols[!missing_vec]

##### read LIBOR daily rates  ######
getSymbols("USD1MTD156N",src="FRED",auto.assign = T,from=startDate,to=endDate)
LIBOR <- na.locf(USD1MTD156N) #replace NAs with prior data
#LIBOR <- USD1MTD156N[!is.na(USD1MTD156N)] #remove NAs
colnames(LIBOR) <- "LIBOR"
LIBOR <- LIBOR[period_filter] #get symbols end date not working?

##### read SP500TR index prices  ######
getSymbols("^SP500TR",src="yahoo", auto.assign = T,from=startDate,to=endDate)
SP500 <- Ad(SP500TR)
colnames(SP500) <- "SP500"

#keep only matching dates
MergedDat <- merge(merge(SP500,LIBOR,join='inner'),Stocks,join='inner')
Stocks <- MergedDat[,-c(1,2)]
# for (i in 1:length(mysymbols)) {
#   Stocks[,i] <- MergedDat[,mysymbols[i]]
# }
SP500 <- MergedDat$SP500
LIBOR <- MergedDat$LIBOR

if (!hist_daily) {                               #keep only end of month
  Stocks <- Stocks[endpoints(Stocks,on="months"),]
  LIBOR  <- LIBOR[endpoints(LIBOR,on="months"),]
  SP500  <- SP500[endpoints(SP500,on="months"),]
  #dates for end of month do not match exactly, 1 and 3 day lag for stocks vs LIBOR
  #Fix by changing end of month dates in LIBOR to match Stocks dates
  #Interest rates don't change much daily so should not be issue, 5/240 entries
  which(is.na(merge(SP500,LIBOR,Stocks)$SP500))
  mismatches <- which(index(LIBOR)!=index(Stocks))
  index(LIBOR)[mismatches] <- index(Stocks)[mismatches]
}

which(is.na(merge(SP500,LIBOR,Stocks)$SP500)) #should be length 0

#Save Rdata variables for calculate module
save(Stocks,file="../DataWork/Stocks.Rdata")
save(LIBOR,file="../DataWork/LIBOR.Rdata")
save(SP500,file="../DataWork/SP500.Rdata")
