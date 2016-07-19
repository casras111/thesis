# test environment for validating main RTR function

DistMat <- expand.grid(mktRet=c(-0.5,-0.2,0.1,0.4,0.7),stockCF=c(60,100,140,180))

testRetMat <- StockPct%*%t(ibm$Returns[1:N])+(1-StockPct)%*%t(MSCI$Returns[1:N])
rownames(RetMat) <- paste("IBMpct",StockPct*100,sep='')
colnames(RetMat) <- paste("Month",1:N,sep='')