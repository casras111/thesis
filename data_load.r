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

ibm <- read.csv(file="ibm_10y.csv")
sp500 <- read.csv(file="sp500_10y.csv")
ibm$Date <- as.Date(as.character(ibm$Date))
sp500$Date <- as.Date(as.character(sp500$Date))

par(mfrow=c(2,1),mar=c(2,4,2,4))
plot(ibm,type="l",main="IBM")
plot(sp500,type="l",main="SP500")


# calculate returns, stddev, correlations and statistic co-distribution
ibm[,3] <- c(log(ibm$Adj.Close[-length(ibm$Adj.Close)]/ibm$Adj.Close[-1]),0)
names(ibm)[3] <- "LnReturns"
sp500[,3] <- c(log(sp500$Adj.Close[-length(sp500$Adj.Close)]/sp500$Adj.Close[-1]),0)
names(sp500)[3] <- "LnReturns"

hist(ibm$LnReturns,prob=T)
curve(dnorm(x,mean=mean(ibm$LnReturns),sd=sd(ibm$LnReturns)),add=T)
hist(sp500$LnReturns,prob=T)
curve(dnorm(x,mean=mean(sp500$LnReturns),sd=sd(sp500$LnReturns)),add=T)
par(mfrow=c(1,1))

StockInitialPrice <- ibm$Adj.Close[120]
StockFinalPrice <- ibm$Adj.Close[1]
MarketInitialPrice <- sp500$Adj.Close[120]
MarketFinalPrice <- sp500$Adj.Close[1]

ibm_AnnVol    <- sd(ibm$LnReturns)*sqrt(12)
sp500_AnnVol  <- sd(sp500$LnReturns)*sqrt(12)
ibm_AnnRet    <- mean(exp(ibm$LnReturns)-1)*12
sp500_AnnRet  <- mean(exp(sp500$LnReturns)-1)*12
sp500_AnnRet2 <- MarketFinalPrice/MarketInitialPrice-1 #absolute 10y return

# risk free annual interest 10y Treasury bills yield 3/1/2005
#source http://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year=2005
rf <-0.0423 
rf10y <- (1+rf)^10-1

Correlation_ibmvssp500 <- cor(ibm$Adj.Close,sp500$Adj.Close)
Cov_ibmvssp500 <- Correlation_ibmvssp500*ibm_AnnVol*sp500_AnnVol

par(mar=c(5,4,4,2))
plot(ibm$LnReturns,sp500$LnReturns,col="blue", pch=20)

plot(c(0,ibm_AnnVol,sp500_AnnVol),c(rf,ibm_AnnRet,sp500_AnnRet),
     xlab="Standard Deviation",ylab="Mean Return",ylim=c(0,0.1))
text(c(0,ibm_AnnVol,sp500_AnnVol),c(rf,ibm_AnnRet,sp500_AnnRet),
     c("Rf","IBM","SP500"),pos=3,cex=0.7)
abline(rf,(sp500_AnnRet-rf)/sp500_AnnVol,col="blue",lty="dotted")

beta_ibmvssp500 <- Correlation_ibmvssp500*ibm_AnnVol/sp500_AnnVol

discount_factor <- 1+rf+beta_ibmvssp500*(sp500_AnnRet-rf)
discount_factor2 <- 1+rf10y+beta_ibmvssp500*(sp500_AnnRet2-rf10y)
estimatedprice_beta <- StockFinalPrice/discount_factor^10
estimatedprice_beta2 <- StockFinalPrice/discount_factor2
diff_beta_price <- StockInitialPrice-estimatedprice_beta

paste(ibm$Date[120],StockInitialPrice)
paste(ibm$Date[1],StockFinalPrice)
estimatedprice_beta

diff_beta_price/StockInitialPrice

Pricei <- 10 #initial stock price, low to get high percentage in portfolio
Xi <- 0.99 #initial percentage of stock in portfolio
Xm <- 1-Xi

Rm <- (MarketFinalPrice/MarketInitialPrice-1)/10

pct_iterationscale <- 0:1000/1000
price_iterationscale <- 10:1000

MinXi <- 1
for (Pricei in price_iterationscale) {
  MaxSharpe <- -1000
  for (Xm in pct_iterationscale) {
    Xi=1-Xm
    Ri <- (StockFinalPrice/Pricei-1)/10
    VarPortfolio <- Xi*ibm_AnnVol^2+Xm*sp500_AnnVol^2+Xi*Xm*Cov_ibmvssp500
    SharpePortfolio <- (Xi*Ri+Xm*Rm-rf)/sqrt(VarPortfolio)
    if (MaxSharpe < SharpePortfolio) {
      MaxSharpe <- SharpePortfolio
      MaxSharpeXi <- Xi
    } 
  }
  if (MinXi > MaxSharpeXi) {
    MinXi <- MaxSharpeXi
    MinXiPricei <- Pricei
    MinXiMaxSharpe <- MaxSharpe
  }
}


MinXiPricei
diff_eq_price <- StockInitialPrice-MinXiPricei
diff_beta_price/StockInitialPrice

InterestRate10YCBOE_url <- "http://real-chart.finance.yahoo.com/table.csv?s=%5ETNX&a=00&b=01&c=2005&d=00&e=01&f=2015&g=m&ignore=.csv"
InterestRate <- yahoo.read(InterestRate10YCBOE_url)
InterestRate[,3] <- c(log(InterestRate$Adj.Close[-length(InterestRate$Adj.Close)]/InterestRate$Adj.Close[-1]),0)
names(InterestRate)[3] <- "LnReturns"

hist(InterestRate$LnReturns,prob=T)
curve(dnorm(x,mean=mean(InterestRate$LnReturns),sd=sd(InterestRate$LnReturns)),add=T)
