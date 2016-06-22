library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)

# Load data from disk: stock, LIBOR interest rate and MSCI global index
ipath <- file.path("../DataRaw","ibm_10y.csv")
ibm <- read.csv(ipath, colClasses = c("Date","numeric"))
lpath <- file.path("../DataWork","LIBOR.csv")
LIBOR <- read.csv(lpath, colClasses = c("Date","numeric"))
mpath <- file.path("../DataWork","MSCI.csv")
MSCI <- read.csv(mpath, colClasses = c("Date","numeric"))

dat <- cbind(LIBOR,MSCI$Price,ibm$Price)
names(dat) <- c("Date","LIBOR","MSCI","IBM")
norm_col <- function(x) x/x[length(x)]  #divide by last entry, oldest
plotdat <- mutate_each(select(dat,Date,MSCI,IBM),funs(norm_col),MSCI,IBM)
plotdat <- melt(plotdat,id="Date",value.name = "Price")
ggplot(plotdat,aes(x=Date,y=Price,colour=variable))+geom_line()+
  ggtitle("MSCI vs IBM scaled returns")

g1 <- ggplot(ibm,aes(x=Date,y=Price))+geom_line()+labs(x="Date",y="Price")+
  ggtitle("IBM stock price")
g2 <- ggplot(MSCI,aes(x=Date,y=Price))+geom_line()+labs(x="Date",y="Price")+
  ggtitle("MSCI index price")
g3 <- ggplot(LIBOR,aes(x=Date,y=Rate))+geom_line()+labs(x="Date",y="Rate")+
  ggtitle("LIBOR rate")
grid.arrange(g1,g2,g3,nrow=3)

# calculate returns, stddev, correlations and statistic co-distribution
#assume data sorted by latest date first
ibm  <- mutate (ibm, Returns=Price/lead(Price)-1) 
MSCI <- mutate (MSCI,Returns=Price/lead(Price)-1)

ggplot(data.frame(msci=MSCI$Return,ibm=ibm$Return))+
  geom_density(aes(x=msci),fill="grey",alpha=0.5)+
  geom_density(aes(x=ibm),fill="blue",alpha=0.2) +ggtitle("Returns distributions")

ibm_MonthVol  <- sd(ibm$Returns,na.rm=TRUE)
msci_MonthVol <- sd(MSCI$Returns,na.rm=TRUE)

ibm_MonthRet  <- mean(ibm$Returns,na.rm=TRUE)
msci_MonthRet <- mean(MSCI$Returns,na.rm=TRUE)

# risk free annual interest 10y Treasury bills yield 3/1/2005
#source http://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year=2005
# rf <-0.0423  # annual, change to monthly series Tbill or LIBOR
# rf <- rf/12 # monthly, placeholder, see above

#plot(c(0,ibm_MonthVol,sp500_MonthVol),c(rf,ibm_MonthRet,sp500_MonthRet),
#     xlab="Monthly Standard Deviation",ylab="Mean Monthly Return",ylim=c(0,0.01))
#text(c(0,ibm_MonthVol,sp500_MonthVol),c(rf,ibm_MonthRet,sp500_MonthRet),
#     c("Rf","IBM","SP500"),pos=3,cex=0.7)
#abline(rf,(sp500_MonthRet-rf)/sp500_MonthVol,col="blue",lty="dotted")

#Expected next period price, naive mean return forward estimation
#ibm <- mutate(ibm,PredictPrice=lead(Price)*(1+ibm_MonthRet))
ibm <- mutate(ibm,PredictPrice=Price*(1+ibm_MonthRet))
#Price taking into account CAPM risk
cov_ibm_msci <- cov(ibm$Returns,MSCI$Returns,use="complete.obs")
beta_ibm <- cov_ibm_msci/(ibm_MonthVol^2)
rf <- LIBOR[NROW(LIBOR),"Rate"]/12/100 #use 2005 first yearly rate, need to fix for monthly
CAPM_discount <- 1+rf+beta_ibm*(msci_MonthRet-rf)
ibm <- mutate (ibm, CAPMPrice=PredictPrice/CAPM_discount)

N <- nrow(ibm)-1 #number of periods, without last one- N/A returns
NWeight <- 1000 #precision for stock weight %, number of discrete steps
StockPct <- 0:NWeight/NWeight #vector for stock % in mixed portfolio
ri_step <- 0.0001 #precision for stock return

#Semivariance implementation
#parameters: x - vector to calculate svar on, r - threshold
svar <- function(x,r=rf) {
  sum(pmin(x-r,0,na.rm=TRUE)^2)/(length(x)-1) #need to fix n for NA?
}

#VAR implementation
#parameters: x - vector to calculate svar on, r - threshold
VAR5pct <- function(x,r=0.05) {
  r5 <- quantile(x,r,names=F)
  if (r5 < 0) {r5 <- (-1)*r5} else r5 <- 0 #NA for positive
  return(r5)
}

#Matrix NWeightxlength(returns) with historical return of all mixed portfolios
#Weighted returns for each period in columns, rows time series return for weight
RetMat <- StockPct%*%t(ibm$Returns[1:N])+(1-StockPct)%*%t(MSCI$Returns[1:N])
rownames(RetMat) <- paste("IBMpct",StockPct*100,sep='')
colnames(RetMat) <- paste("Month",1:N,sep='')

#RTR function returns stock return that has minimum % of stock in portfolio
#optimized for Reward to Risk (RTR) function argument
#Parameters:expected returns,expected price,risk measure to use and initial guess
# RTR <- function(RiskFunc,Rm,Rf,PriceT,PriceGuess=0){
RTR <- function(RiskFunc,Rm,Rf,RiGuess=0.5){
   MinXi <- 1 #stock allocation in optimal portfolio, start with large 100%
   # Ri: stock return that brings to minimum % of stock in optimal market+stock portfolio
   Ri <- RiGuess*2 #start with high return guess, low price of stock
   #Vector of length Nweight of risk of mixed portfolio
   RiskPortfolioVec <- apply(RetMat,1,RiskFunc)
   while ((Ri > 0) && (MinXi>0)) { #stops when arrives at 0% allocation of stock (MinXi)
    #Vector of length Nweight of returns of mixed portfolio for average values
    RetPortfolioVec <- StockPct*Ri+(1-StockPct)*Rm-Rf
    #Vector of length Nweight of Reward to Risk of mixed portfolio
    RTRPortfolio <- RetPortfolioVec/RiskPortfolioVec
    MaxRTRXi <- StockPct[which.max(RTRPortfolio)] #use max RTR index to get stock %
    if (MinXi > MaxRTRXi) {
      MinXi <- MaxRTRXi
      MinRi <- Ri
    }
    Ri <- Ri-ri_step
  }
   return(MinRi)
}

#Rprof()
#vartime <- system.time(
var_discount  <- 1+RTR("sd",     msci_MonthRet,rf,ibm_MonthRet)
svar_discount <- 1+RTR("svar",   msci_MonthRet,rf,ibm_MonthRet)
VAR5_discount <- 1+RTR("VAR5pct",msci_MonthRet,rf,ibm_MonthRet)
for (j in 1:N) {
  #Current month price = earlier month*predicted return based on risk measures
  ibm[j,"Var_Price"]     <- ibm$PredictPrice[j]/var_discount
  ibm[j,"SVar_Price"]    <- ibm$PredictPrice[j]/svar_discount
  ibm[j,"VAR5pct_Price"] <- ibm$PredictPrice[j]/VAR5_discount
}
#)
#Rprof(NULL)

#sum of squares of the error for variance risk predict
MSE2 <- ibm %>% filter(!is.na(Returns)) %>% 
  mutate(err = (Price-Var_Price)^2) %>% summarize(mse=sum(err))
#sum of squares of the error for semivariance risk predict
MSE3 <- ibm %>% filter(!is.na(Returns)) %>% 
  mutate(err = (Price-SVar_Price)^2) %>% summarize(mse=sum(err))
#sum of squares of the error for VAR risk predict
MSE4 <- ibm %>% filter(!is.na(Returns)) %>% 
  mutate(err = (Price-VAR5pct_Price)^2) %>% summarize(mse=sum(err))

sprintf("Variance MSE %.1f, Semivariance MSE %.1f, VAR at 5 pct %.1f",MSE2,MSE3,MSE4)

#Strategy of buy if VAR/SVAR price below market price,sell otherwise, discounted
#not good check, always same profit if always buys
# ibm <- ibm %>% 
#   mutate (ProfitVAR =
#             ifelse(lead(Var_Price)>lead(Adj.Close),
#                    (Adj.Close-lead(Adj.Close)*(1+rf)),
#                    (lead(Adj.Close)*(1+rf)-Adj.Close)),
#           ProfitSVAR =
#             ifelse(lead(SVar_Price)>lead(Adj.Close),
#                    (Adj.Close-lead(Adj.Close)*(1+rf)),
#                    (lead(Adj.Close)*(1+rf)-Adj.Close)))
#total VAR + SVAR profit
# sum(ibm$ProfitVAR,na.rm=TRUE)
# sum(ibm$ProfitSVAR,na.rm=TRUE)
#ibm %>% filter(!is.na(ProfitVAR)) %>% summarize(prVAR=sum(ProfitVAR))

(head(ibm))

#plot(x=colnames(Sharpe_mat),y=Sharpe_mat[MinXiPricei,],type="l",
#     xlab="% market",ylab="Sharpe of portfolio",
#     main=c("Portfolio Sharpe value vs % market for stock price of",MinXiPricei))
#g_tmp <- ggplot(Sharpe_mat,aes(x=colnames(Sharpe_mat),y=Sharpe_mat[MinXi,]))

#paste("Algorithm price",MinXiPricei)

#diff_eq_price <- StockInitialPrice-MinXiPricei
#diff_eq_price/StockInitialPrice