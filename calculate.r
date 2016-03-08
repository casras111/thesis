# Load data from disk
ibm <- read.csv(file="ibm_10y.csv")
sp500 <- read.csv(file="sp500_10y.csv")

ibm$Date <- as.Date(as.character(ibm$Date))
sp500$Date <- as.Date(as.character(sp500$Date))

par(mfrow=c(2,1),mar=c(2,4,2,4))
plot(ibm,type="l",main="IBM")
plot(sp500,type="l",main="SP500")


# calculate returns, stddev, correlations and statistic co-distribution
ibm[,3] <- c(log(ibm$Adj.Close[-nrow(ibm)]/ibm$Adj.Close[-1]),0)
names(ibm)[3] <- "LnReturns"
sp500[,3] <- c(log(sp500$Adj.Close[-nrow(sp500)]/sp500$Adj.Close[-1]),0)
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
paste("estimated CAPM price",estimatedprice_beta)

(diff_beta_price/StockInitialPrice)

Rm <- (MarketFinalPrice/MarketInitialPrice-1)/10

NWeight <- 10
NPrices <- 200

pct_iterationscale <- 0:NWeight/NWeight
price_iterationscale <- 1:NPrices #need to fix for % of original price

Sharpe_mat <- matrix(nrow=NPrices,ncol=NWeight+1)
rownames(Sharpe_mat)<-price_iterationscale
colnames(Sharpe_mat)<-pct_iterationscale

MinXi <- 1
for (Pricei in price_iterationscale) {
  MaxSharpe <- -1000
  for (Xm in pct_iterationscale) {
    Xi=1-Xm
    Ri <- (StockFinalPrice/Pricei-1)/10
    VarPortfolio <- (Xi*ibm_AnnVol)^2+(Xm*sp500_AnnVol)^2+Xi*Xm*Cov_ibmvssp500
    RetPortfolio <- Xi*Ri+Xm*Rm-rf
    SharpePortfolio <- RetPortfolio/sqrt(VarPortfolio)
    Sharpe_mat[Pricei,round(NWeight*Xm)+1] <- SharpePortfolio
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


paste("Algorithm price",MinXiPricei)
# need to check why price did not match exactly CAPM price

diff_eq_price <- StockInitialPrice-MinXiPricei
diff_eq_price/StockInitialPrice