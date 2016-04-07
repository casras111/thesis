library(ggplot2)
library(dplyr)

# Load data from disk
ibm <- read.csv(file="ibm_10y.csv")
sp500 <- read.csv(file="sp500_10y.csv")

ibm$Date <- as.Date(as.character(ibm$Date))
sp500$Date <- as.Date(as.character(sp500$Date))

#par(mfrow=c(2,1),mar=c(2,4,2,4))
#plot(ibm,type="l",main="IBM")
#plot(sp500,type="l",main="SP500")


# calculate returns, stddev, correlations and statistic co-distribution

#ibm[,"LnReturns"] <- c(log(ibm$Adj.Close[-nrow(ibm)]/ibm$Adj.Close[-1]),0)
#names(ibm)[3] <- "LnReturns"
#sp500[,3] <- c(log(sp500$Adj.Close[-nrow(sp500)]/sp500$Adj.Close[-1]),0)
#names(sp500)[3] <- "LnReturns"

#assume data sorted by latest date first
ibm   <- ibm   %>% mutate (Returns=Adj.Close/lead(Adj.Close)-1) 
#                %>% filter(!is.na(Returns))
sp500 <- sp500 %>% mutate (Returns=Adj.Close/lead(Adj.Close)-1)
#              %>% filter(!is.na(Returns))
#ibm[nrow(ibm),"LnReturns"] <- 0 #fix first day NA return to 0
#sp500 <- sp500 %>% mutate (LnReturns=log(Adj.Close)-log(lead(Adj.Close)))
#sp500[nrow(sp500),"LnReturns"] <- 0 #fix first day NA return to 0

#hist(ibm$Returns,prob=T)
#curve(dnorm(x,mean=mean(ibm$Returns),sd=sd(ibm$Returns)),add=T)
#hist(sp500$Returns,prob=T)
#curve(dnorm(x,mean=mean(sp500$Returns),sd=sd(sp500$Returns)),add=T)
#par(mfrow=c(1,1))

#par(mar=c(5,4,4,2))
#plot(ibm$Returns,sp500$Returns,col="blue", pch=20)

StockInitialPrice <- ibm$Adj.Close[120]
StockFinalPrice <- ibm$Adj.Close[1]
MarketInitialPrice <- sp500$Adj.Close[120]
MarketFinalPrice <- sp500$Adj.Close[1]

ibm_MonthVol    <- sd(ibm$Returns,na.rm=TRUE)
sp500_MonthVol  <- sd(sp500$Returns,na.rm=TRUE)
#ibm_AnnVol    <- ibm_MonthVol*sqrt(12)
#sp500_AnnVol  <- sp500_MonthVol*sqrt(12)
#ibm_10yVol    <- ibm_MonthVol*sqrt(120)
#sp500_10yVol  <- sp500_MonthVol*sqrt(120)

ibm_MonthRet    <- mean(ibm$Returns,na.rm=TRUE)
sp500_MonthRet  <- mean(sp500$Returns,na.rm=TRUE)
#ibm_AnnRet    <- ibm_MonthRet*12
#sp500_AnnRet  <- sp500_MonthRet*12
#sp500_10yRet <- MarketFinalPrice/MarketInitialPrice-1 #absolute 10y return

# risk free annual interest 10y Treasury bills yield 3/1/2005
#source http://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year=2005
rf <-0.0423  # annual, change to monthly series Tbill or LIBOR
rfm <- rf/12 # monthly, placeholder, see above
#rf10y <- (1+rf)^10-1

#plot(c(0,ibm_AnnVol,sp500_AnnVol),c(rf,ibm_AnnRet,sp500_AnnRet),
#     xlab="Yearly Standard Deviation",ylab="Yearly Mean Return",ylim=c(0,0.1))
#text(c(0,ibm_AnnVol,sp500_AnnVol),c(rf,ibm_AnnRet,sp500_AnnRet),
#     c("Rf","IBM","SP500"),pos=3,cex=0.7)
#abline(rf,(sp500_AnnRet-rf)/sp500_AnnVol,col="blue",lty="dotted")

#plot(c(0,ibm_MonthVol,sp500_MonthVol),c(rfm,ibm_MonthRet,sp500_MonthRet),
#     xlab="Monthly Standard Deviation",ylab="Mean Monthly Return",ylim=c(0,0.01))
#text(c(0,ibm_MonthVol,sp500_MonthVol),c(rfm,ibm_MonthRet,sp500_MonthRet),
#     c("Rf","IBM","SP500"),pos=3,cex=0.7)
#abline(rfm,(sp500_MonthRet-rfm)/sp500_MonthVol,col="blue",lty="dotted")

#Correlation_ibmvssp500 <- cor(ibm$LnReturns,sp500$LnReturns)
#Cov_ibmvssp500  <- Correlation_ibmvssp500*ibm_MonthVol*sp500_MonthVol

#beta_ibm_sp500 <- Correlation_ibmvssp500*ibm_MonthVol/sp500_MonthVol
  
#discount_factor <- 1+rfm+beta_ibmvssp500*(sp500_MonthRet-rfm)
#discount_factor2 <- 1+rf10y+beta_ibmvssp500*(sp500_10yRet-rf10y)
#estimatedprice_beta <- StockFinalPrice/discount_factor^10
#estimatedprice_beta2 <- StockFinalPrice/discount_factor2
#diff_beta_price <- StockInitialPrice-estimatedprice_beta2


#paste(ibm$Date[120],StockInitialPrice)
#paste(ibm$Date[1],StockFinalPrice)
#paste("estimated CAPM price",estimatedprice_beta2)

#(diff_beta_price/StockInitialPrice)

#Expected price, naive mean return forward estimation
ibm <- ibm %>% mutate (PredictPrice=Adj.Close*(1+ibm_MonthRet))
#Price taking into account CAPM risk
cov_ibm_sp500 <- cov(ibm$Returns,sp500$Returns,use="complete.obs")
beta_ibm <- cov_ibm_sp500/(ibm_MonthVol^2)
CAPM_discount <- 1+rfm+beta_ibm*(sp500_MonthRet-rfm)
ibm <- ibm %>% mutate (CAPMPrice=PredictPrice/CAPM_discount)

NWeight <- 100 #precision for stock weight %, number of discrete steps
pct_iterationscale <- 0:NWeight/NWeight
price_step <- 0.2 #precision for stock price

#Sharpe_mat <- matrix(nrow=NPrices,ncol=NWeight+1)
#rownames(Sharpe_mat)<-price_iterationscale
#colnames(Sharpe_mat)<-pct_iterationscale

#Semivariance implementation
#parameters: x - vector to calculate svar on, r - threshold
svar <- function(x,r) {
  sum(pmin(x-r,0,na.rm=TRUE)^2)/(length(x)-1)
}

#Returns stock price that has minimum % of stock in portfolio optimized
#for Reward to Risk (RTR) function passed to function
#Parameters:expected returns,expected price,risk measure to use and initial guess
RTR <- function(RiskFunc,RetM,RF,PriceT,PriceGuess=0){
  MinXi <- 1
  Pricei <- PriceGuess/2
  while ((Pricei < PriceGuess*2) && (MinXi>0)) {
    MaxRTR <- -1000
    Ri <- PriceT/Pricei-1
    for (Xm in pct_iterationscale) {
      Xi=1-Xm
      #VarPortfolio <- (Xi*ibm_AnnVol)^2+(Xm*sp500_AnnVol)^2+Xi*Xm*Cov_ibmvssp500
      if (RiskFunc=="sd") {
        arglist <- list(Xi*ibm$Returns+Xm*sp500$Returns,na.rm=TRUE)
      } else { #svar
        arglist <- list(Xi*ibm$Returns+Xm*sp500$Returns,RF)
      }
      RiskPortfolio <- do.call(RiskFunc,args=arglist)
      RetPortfolio <- Xi*Ri+Xm*RetM-RF
      RTRPortfolio <- RetPortfolio/RiskPortfolio
      if (MaxRTR < RTRPortfolio) {
        MaxRTR <- RTRPortfolio
        MaxRTRXi <- Xi
      } 
    }
    if (MinXi > MaxRTRXi) {
      MinXi <- MaxRTRXi
      MinXiPricei <- Pricei
      MinXiMaxSharpe <- MaxRTR
    }
    Pricei <- Pricei+price_step
  }
  return(MinXiPricei)
}

vartime <- system.time(
#  ibm <- ibm %>% rowwise() %>% 
#    mutate(VAR_Price =RTR("sd",  sp500_MonthRet,rfm,PredictPrice,Adj.Close),
#           SVAR_Price=RTR("svar",sp500_MonthRet,rfm,PredictPrice,Adj.Close))
#)
 for (j in 1:(nrow(ibm)-1)) {
   ibm[j,"VAR_Price"]  <- RTR("sd",  sp500_MonthRet,rfm,ibm$PredictPrice[j],ibm$Adj.Close[j])
   ibm[j,"SVAR_Price"] <- RTR("svar",sp500_MonthRet,rfm,ibm$PredictPrice[j],ibm$Adj.Close[j])
 }
)

#sum of squares of the error for naive predict
ibm %>% filter(!is.na(Returns)) %>% 
  mutate(err = (Adj.Close-PredictPrice)^2) %>% summarize(mse=sum(err))
#sum of squares of the error for variance risk predict
ibm %>% filter(!is.na(Returns)) %>% 
  mutate(err = (Adj.Close-VAR_Price)^2) %>% summarize(mse=sum(err))

#Strategy of buy if VAR/SVAR price below market price,sell otherwise, discounted
ibm <- ibm %>% 
  mutate (ProfitVAR =
            ifelse(lead(VAR_Price)>lead(Adj.Close),
                   (Adj.Close-lead(Adj.Close)*(1+rfm)),
                   (lead(Adj.Close)*(1+rfm)-Adj.Close)),
          ProfitSVAR =
            ifelse(lead(SVAR_Price)>lead(Adj.Close),
                   (Adj.Close-lead(Adj.Close)*(1+rfm)),
                   (lead(Adj.Close)*(1+rfm)-Adj.Close)))
#total VAR + SVAR profit
sum(ibm$ProfitVAR,na.rm=TRUE)
sum(ibm$ProfitSVAR,na.rm=TRUE)
#ibm %>% filter(!is.na(ProfitVAR)) %>% summarize(prVAR=sum(ProfitVAR))

(head(ibm))

#plot(x=colnames(Sharpe_mat),y=Sharpe_mat[MinXiPricei,],type="l",
#     xlab="% market",ylab="Sharpe of portfolio",
#     main=c("Portfolio Sharpe value vs % market for stock price of",MinXiPricei))
#g_tmp <- ggplot(Sharpe_mat,aes(x=colnames(Sharpe_mat),y=Sharpe_mat[MinXi,]))

#paste("Algorithm price",MinXiPricei)

#diff_eq_price <- StockInitialPrice-MinXiPricei
#diff_eq_price/StockInitialPrice