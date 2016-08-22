# test environment for validating main RTR function

DistMat <- expand.grid(indxRet=c(-0.5,-0.2,0.1,0.4,0.7),StockPrices=c(60,100,140,180))
rf <- 0.04
# NWeight <- 10000 #precision for stock weight %, number of discrete steps
# StockPct <- 0:NWeight/NWeight #vector for stock % in mixed portfolio

cpr <- mean(DistMat$StockPrices) #current price of stock - using mean of prices

#pop.sd <- function (x) {return(sqrt(sum((x-mean(x))^2)/length(x)))}

testbeta <- cov(DistMat$indxRet,DistMat$StockPrices/cpr-1)/var(DistMat$StockPrices/cpr-1)
(CAPMPrice <- cpr/(1+rf+testbeta*(mean(DistMat$indxRet)-rf)))
#should be 115.3846 for NWeight=100000
(VarPrice <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="pop.sd",Rf=rf)$root)
#should be 120.8843
(SVarPrice <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="svar_n",Rf=rf)$root)
#should be 113.5862
(VAR5PctPrice <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="VAR5pct",Rf=rf)$root)
#should be 117.5173
(VAR10PctPrice <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="VAR10pct",Rf=rf)$root)
#should be 114.0436
(VAR20PctPrice <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="VAR20pct",Rf=rf)$root)
