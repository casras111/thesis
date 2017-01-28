# test environment for validating main RTR function

#Case 1 in Kroll - correlation=0, skewness =0
#DistMat <- expand.grid(IndexRet=c(-0.5,-0.2,0.1,0.4,0.7),StockPrices=c(60,100,140,180))
#Case 2 in Kroll - correlation=0.269, skewness 26.79
# DistMat <- expand.grid(IndexRet=c(rep(0.1,2),-0.1),
#                        StockPrices=c(rep(100,2),60))
DistMat <- data.frame(IndexRet=c(0.5,-0.1),
                      StockPrices=c(100,40))
rf <- 0.04
cap_pct <- 0.000001
cpr <- mean(DistMat$StockPrices) #current price of stock - using mean of prices

#check correlation and skewness of returns
cat("correlation of stocks/index:",cor(DistMat$IndexRet,DistMat$StockPrices),"\n")
cat("skewness   of stock returns:",skewness(DistMat$StockPrices),"\n")

#testbeta <- cov(DistMat$IndexRet,DistMat$StockPrices/cpr-1)/var(DistMat$StockPrices/cpr-1)
testbeta <- cov(DistMat$IndexRet,DistMat$StockPrices/cpr-1)/var(DistMat$IndexRet)
cat("CAPM",CAPMPrice <- cpr/(1+rf+testbeta*(mean(DistMat$IndexRet)-rf)),"\n")
#should be 115.3846 for cap_pct=0.000001
cat("VARP",
    VarPrice <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="pop.sd",Rf=rf,DistMat=DistMat)$root,
    "\n")
#should be 130.991
# (SVarPrice <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="svar_n",Rf=rf,DistMat=DistMat)$root)
# #should be 113.5862
# (VAR5PctPrice <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="VAR5pct",Rf=rf,DistMat=DistMat)$root)
# #should be 117.5173
# (VAR10PctPrice <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="VAR10pct",Rf=rf,DistMat=DistMat)$root)
# #should be 118.0603
# (VAR20PctPrice <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="VAR20pct",Rf=rf,DistMat=DistMat)$root)
