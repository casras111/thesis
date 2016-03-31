library(ggplot2)
library(dplyr)

# Load data from disk
ibm <- read.csv(file="ibm_10y.csv")
sp500 <- read.csv(file="sp500_10y.csv")

ibm$Date <- as.Date(as.character(ibm$Date))
sp500$Date <- as.Date(as.character(sp500$Date))

par(mfrow=c(2,1),mar=c(2,4,2,4))
plot(ibm,type="l",main="IBM")
plot(sp500,type="l",main="SP500")


# calculate returns, stddev, correlations and statistic co-distribution

#ibm[,"LnReturns"] <- c(log(ibm$Adj.Close[-nrow(ibm)]/ibm$Adj.Close[-1]),0)
#names(ibm)[3] <- "LnReturns"
#sp500[,3] <- c(log(sp500$Adj.Close[-nrow(sp500)]/sp500$Adj.Close[-1]),0)
#names(sp500)[3] <- "LnReturns"
ibm <- ibm %>% mutate (LnReturns=log(Adj.Close)-log(lead(Adj.Close)))
ibm[nrow(ibm),"LnReturns"] <- 0 #fix first day NA return to 0
sp500 <- sp500 %>% mutate (LnReturns=log(Adj.Close)-log(lead(Adj.Close)))
sp500[nrow(sp500),"LnReturns"] <- 0 #fix first day NA return to 0

hist(ibm$LnReturns,prob=T)
curve(dnorm(x,mean=mean(ibm$LnReturns),sd=sd(ibm$LnReturns)),add=T)
hist(sp500$LnReturns,prob=T)
curve(dnorm(x,mean=mean(sp500$LnReturns),sd=sd(sp500$LnReturns)),add=T)
par(mfrow=c(1,1))

par(mar=c(5,4,4,2))
plot(ibm$LnReturns,sp500$LnReturns,col="blue", pch=20)

StockInitialPrice <- ibm$Adj.Close[120]
StockFinalPrice <- ibm$Adj.Close[1]
MarketInitialPrice <- sp500$Adj.Close[120]
MarketFinalPrice <- sp500$Adj.Close[1]

ibm_MonthVol    <- sd(ibm$LnReturns)
sp500_MonthVol  <- sd(sp500$LnReturns)
ibm_AnnVol    <- ibm_MonthVol*sqrt(12)
sp500_AnnVol  <- sp500_MonthVol*sqrt(12)
ibm_10yVol    <- ibm_MonthVol*sqrt(120)
sp500_10yVol  <- sp500_MonthVol*sqrt(120)

ibm_MonthRet    <- mean(exp(ibm$LnReturns)-1)
sp500_MonthRet  <- mean(exp(sp500$LnReturns)-1)
ibm_AnnRet    <- ibm_MonthRet*12
sp500_AnnRet  <- sp500_MonthRet*12
sp500_10yRet <- MarketFinalPrice/MarketInitialPrice-1 #absolute 10y return

# risk free annual interest 10y Treasury bills yield 3/1/2005
#source http://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year=2005
rf <-0.0423  # annual, change to monthly series Tbill or LIBOR
rfm <- rf/12 # monthly, placeholder, see above
rf10y <- (1+rf)^10-1

plot(c(0,ibm_AnnVol,sp500_AnnVol),c(rf,ibm_AnnRet,sp500_AnnRet),
     xlab="Yearly Standard Deviation",ylab="Yearly Mean Return",ylim=c(0,0.1))
text(c(0,ibm_AnnVol,sp500_AnnVol),c(rf,ibm_AnnRet,sp500_AnnRet),
     c("Rf","IBM","SP500"),pos=3,cex=0.7)
abline(rf,(sp500_AnnRet-rf)/sp500_AnnVol,col="blue",lty="dotted")

plot(c(0,ibm_MonthVol,sp500_MonthVol),c(rfm,ibm_MonthRet,sp500_MonthRet),
     xlab="Monthly Standard Deviation",ylab="Mean Monthly Return",ylim=c(0,0.01))
text(c(0,ibm_MonthVol,sp500_MonthVol),c(rfm,ibm_MonthRet,sp500_MonthRet),
     c("Rf","IBM","SP500"),pos=3,cex=0.7)
abline(rfm,(sp500_MonthRet-rfm)/sp500_MonthVol,col="blue",lty="dotted")

Correlation_ibmvssp500 <- cor(ibm$LnReturns,sp500$LnReturns)
Cov_ibmvssp500  <- Correlation_ibmvssp500*ibm_MonthVol*sp500_MonthVol
beta_ibmvssp500 <- Correlation_ibmvssp500*ibm_MonthVol/sp500_MonthVol

discount_factor <- 1+rfm+beta_ibmvssp500*(sp500_MonthRet-rfm)
discount_factor2 <- 1+rf10y+beta_ibmvssp500*(sp500_10yRet-rf10y)
estimatedprice_beta <- StockFinalPrice/discount_factor^10
estimatedprice_beta2 <- StockFinalPrice/discount_factor2
diff_beta_price <- StockInitialPrice-estimatedprice_beta2

paste(ibm$Date[120],StockInitialPrice)
paste(ibm$Date[1],StockFinalPrice)
paste("estimated CAPM price",estimatedprice_beta2)

(diff_beta_price/StockInitialPrice)

NWeight <- 100
NPrices <- 200

pct_iterationscale <- 0:NWeight/NWeight
price_iterationscale <- 1:NPrices
#need to fix for % of original price and smaller steps

Sharpe_mat <- matrix(nrow=NPrices,ncol=NWeight+1)
rownames(Sharpe_mat)<-price_iterationscale
colnames(Sharpe_mat)<-pct_iterationscale

MinXi <- 1
for (Pricei in price_iterationscale) {
  MaxSharpe <- -1000
  for (Xm in pct_iterationscale) {
    Xi=1-Xm
    Ri <- StockFinalPrice/Pricei-1
    #VarPortfolio <- (Xi*ibm_AnnVol)^2+(Xm*sp500_AnnVol)^2+Xi*Xm*Cov_ibmvssp500
    RiskPortfolio <- sd(Xi*ibm$LnReturns+Xm*sp500$LnReturns)
    RetPortfolio <- Xi*Ri+Xm*sp500_10yRet-rf10y
    SharpePortfolio <- RetPortfolio/RiskPortfolio
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

plot(x=colnames(Sharpe_mat),y=Sharpe_mat[MinXiPricei,],type="l",
     xlab="% market",ylab="Sharpe of portfolio",
     main=c("Portfolio Sharpe value vs % market for stock price of",MinXiPricei))
#g_tmp <- ggplot(Sharpe_mat,aes(x=colnames(Sharpe_mat),y=Sharpe_mat[MinXi,]))

paste("Algorithm price",MinXiPricei)
# need to check why price did not match exactly CAPM price

diff_eq_price <- StockInitialPrice-MinXiPricei
diff_eq_price/StockInitialPrice