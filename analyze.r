#analyze previous runs

library(ggplot2)
library(quantmod)
library(reshape2)
library(gridExtra)
library(moments)    #for skewness function

#load(file="C:/Users/Claudiu/Dropbox/Thesis/Docs/Data/StocksList26092016Daily_5stocks.Rdata")
#load(file="C:/Users/Claudiu/Dropbox/Thesis/Docs/Data/StocksList21092016Bootstrap10000_5stocks.Rdata")

#load(file="C:/Users/Claudiu/Dropbox/Thesis/Docs/Data/StocksList16122016Monthly48stocks.Rdata")
load(file="C:/Users/Claudiu/Dropbox/Thesis/DataWork/StocksList_1_200.Rdata")
temp <- StocksList
load(file="C:/Users/Claudiu/Dropbox/Thesis/DataWork/StocksList_201_400.Rdata")
StocksList <- c(temp,StocksList)
temp <- StocksList
load(file="C:/Users/Claudiu/Dropbox/Thesis/DataWork/StocksList_401_600.Rdata")
StocksList <- c(temp,StocksList)
temp <- StocksList
load(file="C:/Users/Claudiu/Dropbox/Thesis/DataWork/StocksList_601_778.Rdata")
StocksList <- c(temp,StocksList)

stocknames <- names(StocksList)
N <- dim(StocksList[[1]])[1] #number of periods, assumed consistent for all data structures
#constant defining how many months of history to use, 120 for 10y monthly
n_window <- round(N/2)
calc_start <- N-n_window+1
bootcols <- grep("Boot",colnames(StocksList[[1]]))

#vector with names of stocks that have completed runs
completeStocks <- stocknames[!sapply(lapply(StocksList,last),anyNA)]
completeIndx <- (1:length(StocksList))[!sapply(lapply(StocksList,last),anyNA)] #incomplete runs

for (i in completeIndx) {
  #sum of squares of the error for variance risk predict
  RMSE1 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-CAPMPrice)/Price)^2)))
  RMSE2 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-VarPrice)/Price)^2)))
  RMSE3 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-SVarPrice)/Price)^2)))
  RMSE4 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-VAR5pctPrice)/Price)^2)))
  cat(sprintf("%-4s RMSE: CAPM %.4f, Variance %.4f, Semivariance %.4f, VAR5pct %.4f \n",
              stocknames[i],RMSE1,RMSE2,RMSE3,RMSE4))
  StocksList[[i]]$RMSE_Var <- RMSE2
  StocksList[[i]]$RMSE_SVar <- RMSE3
  StocksList[[i]]$RMSE_VAR5Pct <- RMSE4
  StocksList[[i]]$skewavg <- mean(StocksList[[i]]$Skew,na.rm=T)
  StocksList[[i]]$LogReturn <- ROC(StocksList[[i]]$Price,type="continuous",na.pad=F)
  #skewness (cumulative rolling) for n_window back history
  StocksList[[i]]$LogSkew <- rollapply(StocksList[[i]]$LogReturn,FUN=skewness,
                                    width=n_window,na.rm=T)
}

#extract vector of RMSE %
RMSE_Var      <- sapply(lapply(StocksList,last)[completeIndx],function(x) {return(x$RMSE_Var)})
RMSE_SVar     <- sapply(lapply(StocksList,last)[completeIndx],function(x) {return(x$RMSE_SVar)})
RMSE_VAR5Pct  <- sapply(lapply(StocksList,last)[completeIndx],function(x) {return(x$RMSE_VAR5Pct)})
#descriptive statistics in %
RMSE_summary <- 100*rbind(summary(RMSE_Var),summary(RMSE_SVar),summary(RMSE_VAR5Pct))
row.names(RMSE_summary)<-c("Variance","Semivariance","VAR")
RMSE_summary

plot.df <- data.frame(stock=names(RMSE_Var),RMSE_Var,RMSE_SVar,RMSE_VAR5Pct)
plot.df <- melt(plot.df,id="stock",value.name="RMSE",
                variable.name="Risk_Measure")
ggplot(plot.df,aes(Risk_Measure,RMSE))+geom_boxplot()

#boxplot(RMSE_Var,RMSE_SVar,RMSE_VAR5Pct,names=c("Variance","Semivariance","VAR"))

skew_last     <- sapply(lapply(StocksList,last)[completeIndx],function(x) {return(x$Skew)})
skew_first    <- sapply(lapply(StocksList,function(x) {return(first(last(x,120)))})[completeIndx],
                        function(x) {return(x$Skew)})
skew_avg      <- 0.5*(skew_last+skew_first)
skew_rollavg  <- sapply(lapply(StocksList,last)[completeIndx],function(x) {return(x$skewavg)})

skew_log_last     <- sapply(lapply(StocksList,last)[completeIndx],function(x) {return(x$LogSkew)})
skew_log_first    <- sapply(lapply(StocksList,function(x) {return(first(last(x,120)))})[completeIndx],
                        function(x) {return(x$LogSkew)})
skew_log_avg      <- 0.5*(skew_log_last+skew_log_first)

skew_df <- as.data.frame(cbind(skew_last,skew_first,skew_avg,skew_rollavg,skew_log_avg,
                               RMSE_Var,RMSE_SVar))
skew_df$Var_best <- (skew_df$RMSE_Var < skew_df$RMSE_SVar)
skew_df$Predictor_Group <- ifelse((skew_df$RMSE_Var < skew_df$RMSE_SVar),"G1","G2")

reg1 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_first)
summary(reg1)
reg2 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_last)
summary(reg2)
reg3 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_rollavg)
summary(reg3)
reg4 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_avg)
summary(reg4)
reg5 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_log_avg)
summary(reg5)

logreg <- glm(skew_df$Var_best ~ skew_df$skew_avg,family=binomial(link="logit"))
summary(logreg)
ggplot(skew_df,aes(Predictor_Group,skew_avg))+geom_boxplot()+
  ggtitle("Stock returns skewness in optimal RMSE groups")

ggplot(skew_df,aes(Var_best,skew_avg))+geom_boxplot()+
  ggtitle("RMSE Variance < RMSE Semivariance as a function of skewness")
# boxplot(skew_df$skew_avg~skew_df$Var_best)
# title("RMSE Variance < RMSE Semivariance as a function of skewness")

#if data contains bootstrap columns show last period stats
if (!is.null(dim(bootcols))) {
  for (i in seq_along(stocknames)) {
    print(stocknames[i])
    print(StocksList[[i]][N,bootcols])
  }
}

# for (i in completeIndx) {
#   #plot.xts <- StocksList[[i]][calc_start:N,c("Price","CAPMPrice","VarPrice","SVarPrice","VAR5pctPrice")]
#   plot.xts <- StocksList[[i]][calc_start:N,c("Price","VarPrice","SVarPrice","VAR5pctPrice")]
#   plot.df <- data.frame(coredata(plot.xts))
#   plot.df$Date <- index(plot.xts)
#   plotdat2 <- melt(plot.df,id="Date",value.name="PlotPrice")
#   title_string <- paste(stocknames[i],
#                         "actual price Vs risk discount estimated prices")
#   g1<- ggplot(plotdat2,aes(x=Date,y=PlotPrice,colour=variable))+geom_line()+
#     ggtitle(title_string)
#   # print(g1) #not separate enough for visualizing differences
#   pricesdat <- 100*(plot.df[,-c(1,5)]-plot.df$Price)/plot.df$Price
#   pricesdat$Date <- index(plot.xts)
#   plotdat2 <- melt(pricesdat,id="Date",value.name="PlotPrice")
#   title_string <- paste(stocknames[i],
#                         "Risk discount estimated prices vs real price")
#   g2 <- ggplot(plotdat2,aes(x=Date,y=PlotPrice,colour=variable))+geom_line()+
#     labs(y="Price error pct")+
#     ggtitle(title_string)
#   #print(g2)
#   grid.arrange(g1,g2,nrow=2)
# }
