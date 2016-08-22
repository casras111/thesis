library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
library(utils)

# Load data from disk: stock, LIBOR interest rate and MSCI global index
ipath <- file.path("../DataRaw","ibm_10y.csv")
ibm <- read.csv(ipath, colClasses = c("Date","numeric"))
lpath <- file.path("../DataWork","LIBOR.csv")
LIBOR <- read.csv(lpath, colClasses = c("Date","numeric"))
mpath <- file.path("../DataWork","MSCI.csv")
MSCI <- read.csv(mpath, colClasses = c("Date","numeric"))

LIBOR$Rate <- LIBOR$Rate/12/100 #transform to monthly percent

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
N <- nrow(ibm)-1 #number of periods, without last one- N/A returns
ibm <- ibm[1:N,] #remove oldest entry that has NA for returns
MSCI <- MSCI[1:N,]
LIBOR <- LIBOR[1:N,]

ggplot(data.frame(msci=MSCI$Return,ibm=ibm$Return))+
  geom_density(aes(x=msci),fill="grey",alpha=0.5)+
  geom_density(aes(x=ibm),fill="blue",alpha=0.2) +ggtitle("Returns distributions")

ggplot(data.frame(msci=MSCI$Return,ibm=ibm$Return),aes(x=msci,y=ibm))+geom_point()+
  ggtitle("Returns distributions")

ibm_MonthVol  <- sd(ibm$Returns,na.rm=TRUE)
msci_MonthVol <- sd(MSCI$Returns,na.rm=TRUE)

ibm_MonthRet  <- mean(ibm$Returns,na.rm=TRUE)
msci_MonthRet <- mean(MSCI$Returns,na.rm=TRUE)

#previous risk free from annual interest 10y Treasury bills yield
#source http://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year=2005

#Expected next period price, mean return forward estimation
ibm <- mutate(ibm,PredictNextPrice=Price*(1+ibm_MonthRet))
#Price taking into account CAPM risk
cov_ibm_msci <- cov(ibm$Returns,MSCI$Returns,use="complete.obs")
beta_ibm <- cov_ibm_msci/(ibm_MonthVol^2)
CAPM_discount <- 1+LIBOR$Rate+beta_ibm*(msci_MonthRet-LIBOR$Rate)
ibm <- mutate (ibm, CAPMPrice=PredictNextPrice/CAPM_discount)

rfplot <- mean(LIBOR$Rate)  #use arithmetic mean, geometric compound better?
#graph showing CAPM prediction for return of stock vs market and risk free rate
plot(c(0,ibm_MonthVol,msci_MonthVol),c(rfplot,ibm_MonthRet,msci_MonthRet),
    xlab="Monthly Standard Deviation",ylab="Mean Monthly Return",ylim=c(0,0.01))
text(c(0,ibm_MonthVol,msci_MonthVol),c(rfplot,ibm_MonthRet,msci_MonthRet),
    c("rf","IBM","MSCI"),pos=3,cex=0.7)
abline(rfplot,(msci_MonthRet-rfplot)/msci_MonthVol,col="blue",lty="dotted")


NWeight <- 1000 #precision for stock weight %, number of discrete steps
StockPct <- 0:NWeight/NWeight #vector for stock % in mixed portfolio

#Semivariance implementation
#parameters: x - vector to calculate svar on, r - threshold
svar <- function(x,r=rf) {
  sum(pmin(x-r,0,na.rm=TRUE)^2)/(length(x)-1) #need to fix n for NA?
}

#semivariance implementation with division by number of returns below only
svar_n <- function(x,r=rf) {
  x_below <- x[x<r]-r
  sum(x_below^2)/(length(x_below)-1)
}

#VAR implementation
#parameters: x - vector to calculate svar on, r - threshold
VAR5pct <- function(x,r=0.05) {
  r5 <- quantile(x,r,names=F)
  if (r5 < 0) {r5 <- (-1)*r5} else r5 <- 0 #NA for positive
  return(r5)
}

VAR10pct <- function(x,r=0.1) {
  r5 <- quantile(x,r,names=F)
  if (r5 < 0) {r5 <- (-1)*r5} else r5 <- 0 #NA for positive
  return(r5)
}

VAR20pct <- function(x,r=0.2) {
  r5 <- quantile(x,r,names=F)
  if (r5 < 0) {r5 <- (-1)*r5} else r5 <- 0 #NA for positive
  return(r5)
}

#implement population sd for testing - without n-1
pop.sd <- function (x) {return(sqrt(sum((x-mean(x))^2)/length(x)))}

DistMat <- data.frame(indxRet=MSCI$Returns[1:N],StockRet=ibm$Returns[1:N])

#check autocorrelation in time series
pacf(DistMat$StockRet,plot=T,lag.max=12)
pacf(DistMat$indxRet,plot=T,lag.max=12)

#RTRMaxStockPct function returns stock % in portfolio that has max Reward to Risk
#RTR optimized for Reward to Risk (RTR) function argument
# RTRMaxStockPct <- function(PriceGuess,RiskFunc,Rf,cap=0){
#   DistMat$Ret_i <- DistMat$StockPrices/PriceGuess-1      #derived returns
#   #Portfolio returns matrix with row for each stock % and columns for months
#   RetMat <- StockPct%*%t(DistMat$Ret_i)+(1-StockPct)%*%t(DistMat$indxRet)
#   RetPortfolioVec  <- apply(RetMat,1,mean)-Rf  #return mean per %stock - risk free
#   RiskPortfolioVec <- apply(RetMat,1,RiskFunc) #risk measure per %stock
#   RTRPortfolio <- RetPortfolioVec/RiskPortfolioVec #Reward to Risk per %stock
#   MaxRTRXi <- StockPct[which.max(RTRPortfolio)] #use max RTR index to get stock %
#   return(MaxRTRXi)
# }

#calculate reward to risk ratio of stock+index portfolio for different risk functions
#from assumed probability distribution of returns DistMat and assumed price PriceGuess
RTR <- function(StockWeight,PriceGuess,RiskFunc,Rf){
  DistMat$Ret_i <- DistMat$StockPrices/PriceGuess-1  #derived returns
  #Portfolio returns vector
  RetVec <- StockWeight*DistMat$Ret_i+(1-StockWeight)*DistMat$indxRet
  RetPortfolio <- mean(RetVec)-Rf                     #return mean - risk free
  RiskPortfolio <- do.call(RiskFunc,list(x=RetVec))   #risk measure
  RiskPortfolio <- RiskPortfolio^(sign(RetPortfolio)) #Israelsen correction for negative rets
  RTRPortfolio <- ifelse(RiskPortfolio==0,
                         .Machine$integer.max,        #for zero svar positive only returns
                         RetPortfolio/RiskPortfolio)  #Reward to Risk
  return(RTRPortfolio)
}

#calculate percent of stock in stock+index portfolio with maximum RTR
#for given stock price and risk function using RTR function
RTRpctmax <- function(PriceGuess,RiskFunc,Rf) {
  pctmax <- optimize(RTR,c(0,1),PriceGuess=PriceGuess,            #optimize stock% between 0-1
                  RiskFunc=RiskFunc,Rf=Rf,maximum=TRUE,tol=1e-9)$maximum
  return(pctmax)
}

cap_pct <- 0.000001 #percent of capitalization of stock out of total market index
#define convex function that has 0 at the lowest non-zero stock pct resolution
#f <- function(x,...) {RTRMaxStockPct(x,...)-cap_pct}
f <- function(x,...) {RTRpctmax(x,...)-cap_pct}

#Rprof()
pb <- txtProgressBar(min=1,max=N,style=3)
vartime <- system.time(
  for (j in 1:N) {
#for (j in 1:1) {  #for testing only last period
    setTxtProgressBar(pb,j)
    rf <- LIBOR$Rate[j]
    cpr <- ibm[j,"Price"]                            #current price abbreviation
    DistMat$StockPrices <- cpr*(1+DistMat$StockRet)  #distribution of expected prices
    ibm[j,"VarPrice"]     <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="pop.sd",Rf=rf)$root
    ibm[j,"SVarPrice"]    <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="svar",Rf=rf)$root
    ibm[j,"VaR5pctPrice"] <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="VAR5pct",Rf=rf)$root
# trying to debug why CAPM price not identical to VarPrice - BUG
#     calcbeta <- cov(DistMat$StockPrices/cpr-1,MSCI$Returns[1:N])/
#       var(DistMat$StockPrices/cpr-1)
#     ibm[j,"CAPMPrice2"] <- mean(DistMat$StockPrices)/
#       (1+rf+calcbeta*(mean(MSCI$Returns,na.rm=T)-rf))
  }
  ,gcFirst=T)
close(pb)
#Rprof(NULL)

# TBD add regression between history average return and predicted return for different stocks and check R2

#sum of squares of the error for variance risk predict
RMSE1 <- ibm %>% filter(!is.na(Returns)) %>% 
  mutate(err = (Price-CAPMPrice)^2) %>% summarize(mse=sqrt(mean(err)))
#sum of squares of the error for variance risk predict
RMSE2 <- ibm %>% filter(!is.na(Returns)) %>% 
  mutate(err = (Price-VarPrice)^2) %>% summarize(mse=sqrt(mean(err)))
#sum of squares of the error for semivariance risk predict
RMSE3 <- ibm %>% filter(!is.na(Returns)) %>% 
  mutate(err = (Price-SVarPrice)^2) %>% summarize(mse=sqrt(mean(err)))
#sum of squares of the error for VAR risk predict
RMSE4 <- ibm %>% filter(!is.na(Returns)) %>% 
  mutate(err = (Price-VaR5pctPrice)^2) %>% summarize(mse=sqrt(mean(err)))

sprintf("CAPM RMSE %.5f, Variance RMSE %.5f, Semivariance RMSE %.5f, VAR at 5 pct %.5f",
        RMSE1,RMSE2,RMSE3,RMSE4)

#Strategy of buy if VAR/SVAR price below market price,sell otherwise, discounted
#not good check, always same profit if always buys
# ibm <- ibm %>% 
#   mutate (ProfitVAR =
#             ifelse(lead(VarPrice)>lead(Adj.Close),
#                    (Adj.Close-lead(Adj.Close)*(1+rf)),
#                    (lead(Adj.Close)*(1+rf)-Adj.Close)),
#           ProfitSVAR =
#             ifelse(lead(SVarPrice)>lead(Adj.Close),
#                    (Adj.Close-lead(Adj.Close)*(1+rf)),
#                    (lead(Adj.Close)*(1+rf)-Adj.Close)))
#total VAR + SVAR profit
# sum(ibm$ProfitVAR,na.rm=TRUE)
# sum(ibm$ProfitSVAR,na.rm=TRUE)
#ibm %>% filter(!is.na(ProfitVAR)) %>% summarize(prVAR=sum(ProfitVAR))

(head(ibm))

plotdat2 <- melt(select(ibm,Date,Price,CAPMPrice,VarPrice,SVarPrice,VaR5pctPrice),
                 id="Date",value.name="PlotPrice")
ggplot(plotdat2,aes(x=Date,y=PlotPrice,colour=variable))+geom_line()+
  ggtitle("Actual price compared to risk discount estimated prices")
pricesdat <- select(ibm,Date,Price,CAPMPrice,VarPrice,SVarPrice,VaR5pctPrice)
pricesdat[,-1] <- pricesdat[,-1]-pricesdat$Price
plotdat2 <- melt(pricesdat,id="Date",value.name="PlotPrice")
ggplot(plotdat2,aes(x=Date,y=PlotPrice,colour=variable))+geom_line()+
  labs(y="Price difference")+
  ggtitle("Difference of different risk discount estimators from real price")