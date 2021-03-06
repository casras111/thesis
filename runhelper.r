#analyze run for skewness relationship to semivariance risk measure

library(quantmod)

load(file="C:/Users/Claudiu/Dropbox/Thesis/Docs/StocksList16122016Monthly48stocks.Rdata")

#vector with names of stocks that have completed runs
completeStocks <- names(StocksList)[sapply(lapply(StocksList,last),length)==11]
completeStocks <- completeStocks[c(-7,-29)]   #incomplete run MIN,UVSP
completeIndx <- (1:length(StocksList))[sapply(lapply(StocksList,last),length)==11]
completeIndx <- completeIndx[c(-7,-29)] #incomplete runs

N <- dim(StocksList[[1]])[1] #number of periods assumed consistent for all data structures
#constant defining how many months of history to use, 120 for 10y monthly
n_window <- round(N/2)
calc_start <- N-n_window+1

for (i in completeIndx) {
  #sum of squares of the error for variance risk predict
  RMSE1 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-CAPMPrice)/Price)^2)))
  RMSE2 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-VarPrice)/Price)^2)))
  RMSE3 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-SVarPrice)/Price)^2)))
  RMSE4 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-VAR5pctPrice)/Price)^2)))
  cat(sprintf("%-4s RMSE: CAPM %.4f, Variance %.4f, Semivariance %.4f, VAR5pct %.4f ,Var_best %-4s, Skew %.2f\n",
              names(StocksList)[i],RMSE1,RMSE2,RMSE3,RMSE4,RMSE2<RMSE3,last(StocksList[[i]])$Skew))
  StocksList[[i]]$RMSE_Var <- RMSE2
  StocksList[[i]]$RMSE_SVar <- RMSE3
  StocksList[[i]]$skewavg <- mean(StocksList[[i]]$Skew,na.rm=T)
}

skew_last     <- sapply(sapply(StocksList,last)[completeIndx],function(x) {return(x$Skew)})
skew_first    <- sapply(sapply(StocksList,function(x) {return(first(last(x,120)))})[completeIndx],
                    function(x) {return(x$Skew)})
skew_avg      <- 0.5*(skew_last+skew_first)
skew_rollavg  <- sapply(sapply(StocksList,last)[completeIndx],function(x) {return(x$skewavg)})
RMSE_Var      <- sapply(sapply(StocksList,last)[completeIndx],function(x) {return(x$RMSE_Var)})
RMSE_SVar     <- sapply(sapply(StocksList,last)[completeIndx],function(x) {return(x$RMSE_SVar)})

skew_df <- as.data.frame(cbind(skew_last,skew_first,skew_avg,skew_rollavg,RMSE_Var,RMSE_SVar))
skew_df$Var_best <- (skew_df$RMSE_Var < skew_df$RMSE_SVar)

tt <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_first)
summary(tt)
tt2 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_rollavg)
summary(tt2)
tt3 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_avg)
summary(tt3)
boxplot(skew_df$skew_avg~skew_df$Var_best)
title("RMSE Variance < RMSE Semivariance as a function of skewness")
