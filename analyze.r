#analyze previous runs

library(ggplot2)
library(quantmod)
library(reshape2)
library(gridExtra)

load(file="C:/Users/Claudiu/Dropbox/Thesis/Docs/StocksList26092016Daily_5stocks.Rdata")
#load(file="C:/Users/Claudiu/Dropbox/Thesis/Docs/StocksList21092016Bootstrap10000_5stocks.Rdata")
#run_bootstrap=FALSE

load("../DataWork/Stocks.Rdata")
stocknames <- colnames(Stocks)
N <- dim(StocksList[[1]])[1] #number of periods assumed consistent for all data structures
#constant defining how many months of history to use, 120 for 10y monthly
n_window <- round(N/2)
calc_start <- N-n_window+1
bootcols <- grep("Boot",colnames(StocksList[[1]]))

for (i in seq_along(stocknames)) {
  #sum of squares of the error for variance risk predict
  RMSE1 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-CAPMPrice)/Price)^2)))
  RMSE2 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-VarPrice)/Price)^2)))
  RMSE3 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-SVarPrice)/Price)^2)))
  RMSE4 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-VAR5pctPrice)/Price)^2)))
  cat(sprintf("%-4s RMSE: CAPM %.4f, Variance %.4f, Semivariance %.4f, VAR5pct %.4f \n",
              stocknames[i],RMSE1,RMSE2,RMSE3,RMSE4))
}

for (i in seq_along(stocknames)) {
  print(stocknames[i])
  print(StocksList[[i]][N,bootcols])
}

for (i in seq_along(stocknames)) {
  plot.xts <- StocksList[[i]][calc_start:N,c("Price","CAPMPrice","VarPrice","SVarPrice","VAR5pctPrice")]
  plot.df <- data.frame(coredata(plot.xts))
  plot.df$Date <- index(plot.xts)
  plotdat2 <- melt(plot.df,id="Date",value.name="PlotPrice")
  title_string <- paste(stocknames[i],
                        "Actual price Vs risk discount estimated prices")
  g1<- ggplot(plotdat2,aes(x=Date,y=PlotPrice,colour=variable))+geom_line()+
    ggtitle(title_string)
  # print(g1) #not separate enough for visualizing differences
  pricesdat <- 100*(plot.df[,-c(1,6)]-plot.df$Price)/plot.df$Price
  pricesdat$Date <- index(plot.xts)
  plotdat2 <- melt(pricesdat,id="Date",value.name="PlotPrice")
  title_string <- paste(stocknames[i],
                        "Risk discount estimated prices vs real price")
  g2 <- ggplot(plotdat2,aes(x=Date,y=PlotPrice,colour=variable))+geom_line()+
    labs(y="Price error pct")+
    ggtitle(title_string)
  #print(g2)
  grid.arrange(g1,g2,nrow=2)
}
