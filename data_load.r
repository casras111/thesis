# Read Yahoo data

# Get SP500 and IBM stock data from Yahoo Finance from 1/1/2005 to 1/1/2015, monthly
ibm_url   <- "http://real-chart.finance.yahoo.com/table.csv?s=IBM&a=00&b=01&c=2005&d=00&e=01&f=2015&g=m&ignore=.csv"
sp500_url <- "http://real-chart.finance.yahoo.com/table.csv?s=%5EGSPC&a=00&b=01&c=2005&d=00&e=01&f=2015&g=m&ignore=.csv"

# Cut only adjusted close column
yahoo.read <- function(url){
  dat <- read.table(url,header=TRUE,sep=",")
  df <- dat[,c(1,7)]
  df$Date <- as.Date(as.character(df$Date))
  return(df)}

ibm  <- yahoo.read(ibm_url)
sp500 <- yahoo.read(sp500_url)

write.csv(ibm,file="ibm_10y.csv",row.names = F)
write.csv(sp500,file="sp500_10y.csv",row.names = F)