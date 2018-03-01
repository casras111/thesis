library(PerformanceAnalytics)
library(ggplot2)

load(file="../DataWork/StocksList_after_Analyze.Rdata")
load(file="../DataWork/SP500.Rdata")

startDate <- "1996-1-1"
midDate   <- "2005-12-31"
midDate_1 <- "2006-1-1"
endDate   <- "2015-12-31"

risknames <- c("All","Var","SVar","VAR","CPT","AVAR","Mix")

for (i in 1:length(StocksList)) {
  StocksList[[i]]$UndervaluedVar <- (StocksList[[i]]$Price < StocksList[[i]]$VarPrice)
  StocksList[[i]]$UndervaluedSVar <- (StocksList[[i]]$Price < StocksList[[i]]$SVarPrice)
  StocksList[[i]]$UndervaluedVAR <- (StocksList[[i]]$Price < StocksList[[i]]$VAR5pctPrice)
  StocksList[[i]]$UndervaluedCPT <- (StocksList[[i]]$Price < StocksList[[i]]$CPTPrice)
  StocksList[[i]]$UndervaluedAVAR <- (StocksList[[i]]$Price < StocksList[[i]]$AVARPrice)
}

#test of Performance Analytics package functions
# tstock <- StocksList[[321]]
# plotdf <- data.frame(index(tstock),coredata(tstock$UndervaluedAVAR))
# names(plotdf) <- c("Date","Val")
# ggplot(plotdf,aes(Date,Val))+geom_line()
# 
# chart.CumReturns(tstock$Return)
# chart.Boxplot(tstock$Return)
# chart.QQPlot(tstock$Return)
# chart.Histogram(tstock$Return)
# table.Stats(tstock$Return)
# 
# ptest <- xts(order.by=(Sys.Date()-10:13),c(0.1,0.4,-0.1,0.5))
# names(ptest) <- "R"
# ptest$Cash <- rep(0,4)
# 
# w <- xts(order.by=(Sys.Date()-10:13),c(1,1,1,1))
# w <- cbind(w,1-w)
# Return.portfolio(ptest,wealth.index = T,verbose = T,weights = w)
#End of test

StocksList <- lapply(StocksList,na.omit)

cat("\nSkew descriptive statistics\n")
print(summary(sapply(lapply(StocksList,last),function(x) {return(x$skewavg)})))
MIDSKEW <- 0.5

ret_mat <- StocksList[[1]]$Return*0
names(ret_mat) <- "CashRet"

w_All <- w_Var <- w_SVar <- w_VAR <- w_CPT <- w_AVAR <- w_Mix <- ret_mat 
names(w_All) <- names(w_Var) <- names(w_SVar) <- names(w_VAR) <- 
  names(w_CPT) <- names(w_AVAR) <- names(w_Mix) <- "Cash"

#build return matrix of all stocks and weights for strategy of long only
#undervalued stocks for each risk measure pricing
for (i in 1:length(StocksList)) {
  ret_mat <- merge(ret_mat,StocksList[[i]]$Return)
  w_All  <- merge(w_All,1)
  w_Var  <- merge(w_Var,StocksList[[i]]$UndervaluedVar)
  w_SVar <- merge(w_SVar,StocksList[[i]]$UndervaluedSVar)
  w_VAR  <- merge(w_VAR,StocksList[[i]]$UndervaluedVAR)
  w_CPT  <- merge(w_CPT,StocksList[[i]]$UndervaluedCPT)
  w_AVAR <- merge(w_AVAR,StocksList[[i]]$UndervaluedAVAR)
  w_Mix  <- merge(w_Mix,ifelse(StocksList[[i]]$skewavg > MIDSKEW,
                               StocksList[[i]]$UndervaluedSVar,
                               StocksList[[i]]$UndervaluedVar))
}

names(w_All)[-1] <- names(ret_mat)[-1] <- names(StocksList)
names(w_Var) <- names(w_SVar) <- names(w_VAR) <- names(w_All)
names(w_CPT) <- names(w_AVAR) <- names(w_Mix) <- names(w_All)

#normalize weights
w_All[,-1] <- w_All[,-1]/rowSums(w_All[,-1])
w_All[,1] <- 1-rowSums(w_All[,-1]) #move to cash if no buy signal
index(w_All) <- index(w_All)-1

w_Var[,-1] <- w_Var[,-1]/rowSums(w_Var[,-1])
w_Var[,1] <- 1-rowSums(w_Var[,-1]) #move to cash if no buy signal
index(w_Var) <- index(w_Var)-1

w_SVar[,-1] <- w_SVar[,-1]/rowSums(w_SVar[,-1])
w_SVar[,1] <- 1-rowSums(w_SVar[,-1]) #move to cash if no buy signal
index(w_SVar) <- index(w_SVar)-1

w_VAR[,-1] <- w_VAR[,-1]/rowSums(w_VAR[,-1])
w_VAR[,1] <- 1-rowSums(w_VAR[,-1]) #move to cash if no buy signal
index(w_VAR) <- index(w_VAR)-1

w_CPT[,-1] <- w_CPT[,-1]/rowSums(w_CPT[,-1])
w_CPT[,1] <- 1-rowSums(w_CPT[,-1]) #move to cash if no buy signal
index(w_CPT) <- index(w_CPT)-1

w_AVAR[,-1] <- w_AVAR[,-1]/rowSums(w_AVAR[,-1])
w_AVAR[,1] <- 1-rowSums(w_AVAR[,-1]) #move to cash if no buy signal
index(w_AVAR) <- index(w_AVAR)-1

w_Mix[,-1] <- w_Mix[,-1]/rowSums(w_Mix[,-1])
w_Mix[,1] <- 1-rowSums(w_Mix[,-1]) #move to cash if no buy signal
index(w_Mix) <- index(w_Mix)-1

ret <- Return.portfolio(ret_mat,weights = w_All)
for (i in risknames[-1]) {
  temp_ret <- Return.portfolio(ret_mat,weights = get(paste0("w_",i)))
  ret <- merge(ret,temp_ret)
}
names(ret) <- risknames

chart.CumReturns(ret,legend.loc = "topleft",wealth.index = T,
                 main="Cumulative Returns")
#cumulative return compare for strategies
cat("\nLong only strategy cumulative return\n")
print(last(Return.portfolio(ret,wealth.index = TRUE,verbose = TRUE)$EOP.Value)*ncol(ret))

#build weights for strategy of short/long for
#undervalued stocks for each risk measure pricing
w_All <- w_Var <- w_SVar <- w_VAR <- w_CPT <- w_AVAR <- w_Mix <- StocksList[[1]]$Return*0 

for (i in 1:length(StocksList)) {
  w_All  <- merge(w_All,1)
  w_Var  <- merge(w_Var, 2*(StocksList[[i]]$UndervaluedVar-0.5))
  w_SVar <- merge(w_SVar,2*(StocksList[[i]]$UndervaluedSVar-0.5))
  w_VAR  <- merge(w_VAR, 2*(StocksList[[i]]$UndervaluedVAR-0.5))
  w_CPT  <- merge(w_CPT, 2*(StocksList[[i]]$UndervaluedCPT-0.5))
  w_AVAR <- merge(w_AVAR,2*(StocksList[[i]]$UndervaluedAVAR-0.5))
  w_Mix  <- merge(w_Mix,ifelse(StocksList[[i]]$skewavg > MIDSKEW,
                               2*(StocksList[[i]]$UndervaluedSVar-0.5),
                               2*(StocksList[[i]]$UndervaluedVar-0.5)))
}

#normalize weights
w_All[,-1] <- w_All[,-1]/rowSums(w_All[,-1])
w_All[,1] <- 1-rowSums(w_All[,-1]) #move to cash if no buy signal
index(w_All) <- index(w_All)-1

w_Var[,-1] <- w_Var[,-1]/rowSums(w_Var[,-1])
w_Var[,1] <- 1-rowSums(w_Var[,-1]) #move to cash if no buy signal
index(w_Var) <- index(w_Var)-1

w_SVar[,-1] <- w_SVar[,-1]/rowSums(w_SVar[,-1])
w_SVar[,1] <- 1-rowSums(w_SVar[,-1]) #move to cash if no buy signal
index(w_SVar) <- index(w_SVar)-1

w_VAR[,-1] <- w_VAR[,-1]/rowSums(w_VAR[,-1])
w_VAR[,1] <- 1-rowSums(w_VAR[,-1]) #move to cash if no buy signal
index(w_VAR) <- index(w_VAR)-1

w_CPT[,-1] <- w_CPT[,-1]/rowSums(w_CPT[,-1])
w_CPT[,1] <- 1-rowSums(w_CPT[,-1]) #move to cash if no buy signal
index(w_CPT) <- index(w_CPT)-1

w_AVAR[,-1] <- w_AVAR[,-1]/rowSums(w_AVAR[,-1])
w_AVAR[,1] <- 1-rowSums(w_AVAR[,-1]) #move to cash if no buy signal
index(w_AVAR) <- index(w_AVAR)-1

w_Mix[,-1] <- w_Mix[,-1]/rowSums(w_Mix[,-1])
w_Mix[,1] <- 1-rowSums(w_Mix[,-1]) #move to cash if no buy signal
index(w_Mix) <- index(w_Mix)-1

ret <- Return.portfolio(ret_mat,weights = w_All)
for (i in risknames[-1]) {
  temp_ret <- Return.portfolio(ret_mat,weights = get(paste0("w_",i)))
  ret <- merge(ret,temp_ret)
}
names(ret) <- risknames

chart.CumReturns(ret,legend.loc = "topleft",wealth.index = T,
                 main="Cumulative Returns")
#cumulative return compare for strategies
cat("\nShort/Long strategy cumulative return\n")
print(last(Return.portfolio(ret,wealth.index = TRUE,verbose = TRUE)$EOP.Value)*ncol(ret))

ret_sp500 <- last(Return.calculate(SP500,method="discrete"),nrow(ret))
chart.CumReturns(ret_sp500,wealth.index = T,main="SP500 Cumulative Returns")
cat("\nSP500 cumulative return\n")
print(last(Return.portfolio(ret_sp500,wealth.index = TRUE,verbose = TRUE)$EOP.Value))

