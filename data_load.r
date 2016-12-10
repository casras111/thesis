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
allstocks <- read.table("../DataRaw/companies.csv",sep=",",stringsAsFactors = F)
mysymbols <- sample(allstocks[,2],50)
getSymbols(mysymbols,src="yahoo", auto.assign = T,from=startDate,to=endDate)
s1 <- Ad(get(mysymbols[1])) #keep only Adjusted.Close, get for string to var
Stocks <- s1
for (i in 2:length(mysymbols)) {
  Stocks <- merge.xts(Stocks,Ad(get(mysymbols[i])))
}
colnames(Stocks) <- mysymbols

##### read LIBOR daily rates  ######
getSymbols("USD1MTD156N",src="FRED",auto.assign = T,from=startDate,to=endDate)
LIBOR <- na.locf(USD1MTD156N) #replace NAs with prior data
#LIBOR <- USD1MTD156N[!is.na(USD1MTD156N)] #remove NAs
colnames(LIBOR) <- "LIBOR"
LIBOR <- LIBOR[period_filter] #get symbols end date not working?

##### read MSCI index monthly prices  ######
# mpath <- file.path("../DataRaw","MSCI_ACWI.xls")
# MSCI <- read.xlsx(file=mpath,sheetIndex=1,startRow = 7,endRow = 346)
# MSCI <- xts(MSCI[,2],MSCI$Date)
# colnames(MSCI) <- "MSCI"
# MSCI <- MSCI[period_filter]

##### read SP500TR index prices  ######
getSymbols("^SP500TR",src="yahoo", auto.assign = T,from=startDate,to=endDate)
SP500 <- Ad(SP500TR)
colnames(SP500) <- "SP500"

#keep only matching dates
MergedDat <- merge(merge(SP500,LIBOR,join='inner'),Stocks,join='inner')
for (i in 1:length(mysymbols)) {
  Stocks[,i] <- MergedDat[,mysymbols[i]]
}
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

#indexcap <- 19000 #$19000 milliard SP500
#cap <- c(152.5,580.5,0.02197,0.00887,0.77376) #2014 EOY market caps
#cap_pct <- cap/indexcap
#names(cap_pct) <- mysymbols

#Save Rdata variables for calculate module
save(Stocks,file="../DataWork/Stocks.Rdata")
save(LIBOR,file="../DataWork/LIBOR.Rdata")
#save(MSCI,file="../DataWork/MSCI.Rdata")
save(SP500,file="../DataWork/SP500.Rdata")
#save(cap_pct,file="../DataWork/cap_pct.Rdata")
