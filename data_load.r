library(dplyr)
library(xlsx)
#library(ggplot2)

#### Read Yahoo stock monthly data  ###

# Read only adjusted close column and date
yahoo.read <- function(url){
  dat <- read.table(url,header=TRUE,sep=",")
  df <- dat[,c(1,7)]
  df$Date <- as.Date(as.character(df$Date))
  return(df)
}

# Get IBM stock data from Yahoo Finance from 1/1/2005 to 1/1/2015, daily
yahoourl <- "http://real-chart.finance.yahoo.com/table.csv?"
dateurl <- "&a=00&b=01&c=2005&d=00&e=01&f=2015&g=d" #1/1/2005-1/1/2015, daily
ibm_url   <- paste0(yahoourl,"s=IBM",dateurl)
ibm  <- yahoo.read(ibm_url)
names(ibm) <- c("Date","Price")
ibm <- arrange(ibm,desc(Date)) #default for yahoo, added only for robustness
#filter only for monthly data, end of month day
ibm <- ibm %>% mutate(y=as.POSIXlt(Date)$year+1900,m=as.POSIXlt(Date)$mon+1)
ibm <- ibm %>% group_by(y,m) %>% slice(1) %>% ungroup() %>% arrange(desc(Date))
ibm <- select(ibm,Date,Price)

write.csv(ibm,file="../DataRaw/ibm_10y.csv",row.names = F)

#sp500_url <- "http://real-chart.finance.yahoo.com/table.csv?s=%5EGSPC&a=00&b=01&c=2005&d=00&e=01&f=2015&g=m&ignore=.csv"
#sp500 <- yahoo.read(sp500_url)
#write.csv(sp500,file="../DataRaw/sp500_10y.csv",row.names = F)

##### read LIBOR daily rates  ######
lpath <- file.path("../DataRaw","LIBOR_1m_2005_2016.csv")
LIBOR <- read.csv(file=lpath, stringsAsFactors = F)
LIBOR[,1] <- as.Date(LIBOR[,1])
LIBOR[,2] <- suppressWarnings(as.numeric(LIBOR[,2]))
#remove NAs
LIBOR <- LIBOR[!is.na(LIBOR[,2]),]
names(LIBOR) <- c("Date","Rate")
#keep only 2005 to 2015 and order dates in descending order
LIBOR <- LIBOR %>% filter((Date >= "2005-01-01") & (Date < "2015-01-01")) %>% 
                   arrange(desc(Date))
#ggplot(LIBOR, aes(y=Rate,x=Date))+geom_line()

#filter only for monthly data, end of month day
LIBOR <- LIBOR %>% mutate(y=as.POSIXlt(Date)$year+1900,m=as.POSIXlt(Date)$mon+1)
LIBOR <- LIBOR %>% group_by(y,m) %>% slice(1) %>% ungroup() %>% arrange(desc(Date))
LIBOR <- select(LIBOR,Date,Rate)

write.csv(LIBOR,file="../DataWork/LIBOR.csv",row.names = F)

##### read MSCI index monthly prices  ######

mpath <- file.path("../DataRaw","MSCI_ACWI.xls")
MSCI <- read.xlsx(file=mpath,sheetIndex=1,startRow = 7,endRow = 346)
names(MSCI) <- c("Date","Price")
#keep only 2005 to 2015 and order dates in descending order
MSCI <- MSCI %>% filter((Date >= "2005-01-01") & (Date < "2015-01-01")) %>% 
  arrange(desc(Date))
#ggplot(MSCI, aes(y=Price,x=Date))+geom_line()
write.csv(MSCI,file="../DataWork/MSCI.csv",row.names = F)

#dates for end of month do not match exactly, 1 and 3 day lag for ibm vs msci
filter(cbind(ibm=ibm,msci=MSCI,libor=LIBOR),(ibm.Date!=msci.Date)|(ibm.Date!=libor.Date))

