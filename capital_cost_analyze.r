#capital cost derivation from Stocklist results

library(ggplot2)
library(quantmod)
library(reshape2)
library(gridExtra)
#library(moments)    #for skewness function

# startDate <- "1996-1-1"
# midDate   <- "2005-12-31"
# midDate_1 <- "2006-1-1"
# endDate   <- "2015-12-31"

#set path according to computer used
if (file.exists("C:/Users/Claudiu")) {
  dropbox_path <- "C:/Users/Claudiu/Dropbox/Thesis/DataWork/"
} else {
  if (file.exists("C:/Users/user")) {
    dropbox_path <- "C:/Users/user/Dropbox/Thesis/DataWork/"
  } else stop("stopping, data directory does not exist")
}

load(file=paste0(dropbox_path,"StocksList.Rdata"))

# Load data from disk: stocks, LIBOR interest rate and SP500 index
load("../DataWork/LIBOR.Rdata")
load("../DataWork/SP500.Rdata")
N <- dim(LIBOR)[1] #number of periods assumed consistent for all data structures
#constant defining how many months of history to use, 120 for 10y monthly
n_window <- round(N/2)
colnames(SP500) <- "Price"
SP500$Return <- ROC(SP500$Price,type="discrete",na.pad=F) #Rate of change, return %
SP500$AvgRet <- rollapply(SP500$Return,FUN=mean,width=n_window)

for (i in 1:length(StocksList)) {
  StocksList[[i]]$beta_Var  <- with(StocksList[[i]],
                                    (PredictNextPrice/VarPrice -1-LIBOR)/(SP500$AvgRet-LIBOR))
  StocksList[[i]]$beta_SVar <- with(StocksList[[i]],
                                    (PredictNextPrice/SVarPrice-1-LIBOR)/(SP500$AvgRet-LIBOR))
}



# #descriptive statistics for 2 periods, average for all stocks for all months
# mean_func <- function(x,a,b) {return(mean(coredata(x[paste0(a,"/",b)]$Return),na.rm=T))}
# sd_func   <- function(x,a,b) {return(sd(coredata(x[paste0(a,"/",b)]$Return),na.rm=T))}
# skew_func <- function(x,a,b) {return(skewness(coredata(x[paste0(a,"/",b)]$Return),na.rm=T))}
# StocksStat1 <- rbind(mean(sapply(StocksList, mean_func,startDate,midDate)),
#                     mean(sapply(StocksList, sd_func,   startDate,midDate)),
#                     mean(sapply(StocksList, skew_func, startDate,midDate)))
# StocksStat2 <- rbind(mean(sapply(StocksList, mean_func,midDate_1,endDate)),
#                      mean(sapply(StocksList, sd_func,  midDate_1,endDate)),
#                      mean(sapply(StocksList, skew_func,midDate_1,endDate)))
# StocksStat <- cbind(StocksStat1,StocksStat2)
# row.names(StocksStat) <- c("Mean","Std Dev","Skewness")
# colnames(StocksStat)  <- c("1995-2005","2006-2015")
# print(StocksStat)
# 
# #descriptive statistics - histogram for monthly cross-section of stocks returns
# statret <- sapply(StocksList,function(x) {return(coredata(x$Return))})
# stat_df1 <- data.frame(Dates=index(StocksList[[1]]),
#                       Mean=apply(statret,1,mean))
# stat_df1 <- stat_df1[-1,]  #remove first NA row
# ggplot(stat_df1,aes(Mean))+geom_histogram(binwidth=0.03)
# #histograms for std dev and skewness
# stat_df2 <- data.frame(StdDev=apply(statret,2,sd,na.rm=T),
#                        Skewness=apply(statret,2,skewness,na.rm=T))
# ggplot(stat_df2,aes(StdDev))+geom_histogram(binwidth=0.02)
# ggplot(stat_df2,aes(Skewness))+geom_histogram(binwidth=0.3)
# 
# 
# #extract vector of RMSE %
# RMSE_Var      <- sapply(lapply(StocksList,last),function(x) {return(x$RMSE_Var)})
# RMSE_SVar     <- sapply(lapply(StocksList,last),function(x) {return(x$RMSE_SVar)})
# RMSE_VAR5Pct  <- sapply(lapply(StocksList,last),function(x) {return(x$RMSE_VAR5Pct)})
# #descriptive statistics in %
# RMSE_summary <- 100*rbind(summary(RMSE_Var),summary(RMSE_SVar),summary(RMSE_VAR5Pct))
# row.names(RMSE_summary)<-c("Variance","Semivariance","VAR")
# RMSE_summary
# 
# plot.df <- data.frame(stock=names(RMSE_Var),Variance=RMSE_Var,
#                       Semivariance=RMSE_SVar,VaR=RMSE_VAR5Pct)
# plot.df <- melt(plot.df,id="stock",value.name="RMSE",
#                 variable.name="Risk_Measure")
# ggplot(plot.df,aes(Risk_Measure,RMSE))+geom_boxplot()
# 
# #boxplot(RMSE_Var,RMSE_SVar,RMSE_VAR5Pct,names=c("Variance","Semivariance","VAR"))
# 
# skew_last     <- sapply(lapply(StocksList,last),function(x) {return(x$Skew)})
# skew_first    <- sapply(lapply(StocksList,function(x) {return(first(last(x,120)))}),
#                         function(x) {return(x$Skew)})
# skew_avg      <- 0.5*(skew_last+skew_first)
# skew_rollavg  <- sapply(lapply(StocksList,last),function(x) {return(x$skewavg)})
# 
# skew_log_last     <- sapply(lapply(StocksList,last),function(x) {return(x$LogSkew)})
# skew_log_first    <- sapply(lapply(StocksList,function(x) {return(first(last(x,120)))}),
#                         function(x) {return(x$LogSkew)})
# skew_log_avg      <- 0.5*(skew_log_last+skew_log_first)
# 
# skew_df <- as.data.frame(cbind(skew_last,skew_first,skew_avg,skew_rollavg,skew_log_avg,
#                                RMSE_Var,RMSE_SVar))
# skew_df$Var_best <- (skew_df$RMSE_Var < skew_df$RMSE_SVar)
# #Labeling G1 group with lower variance RMSE and G2 for semivariance
# skew_df$Predictor_Group <- ifelse((skew_df$RMSE_Var < skew_df$RMSE_SVar),
#                                   "G1","G2")
# ggplot(skew_df,aes(Predictor_Group,skew_avg))+geom_boxplot()
# ggplot(skew_df,aes(skew_avg))+geom_density(aes(colour=Predictor_Group))
# 
# #  ggtitle("Stock returns skewness in optimal RMSE groups")
# summary(skew_df$skew_avg)
# summary(skew_df[skew_df$Predictor_Group=="G1",]$skew_avg)
# summary(skew_df[skew_df$Predictor_Group=="G2",]$skew_avg)
# table(skew_df$Predictor_Group)
# skew_df$positive_skew <- ifelse(skew_df$skew_avg > 0.2,"Large","Small")
# with(skew_df,table(Predictor_Group,positive_skew))
# 
# skew_df$beta <- sapply(lapply(StocksList,last),function(x) {return(x$Beta)})
# table(skew_df$Var_best,skew_df$beta>1.5) 
# 
# reg1 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_first)
# summary(reg1)
# reg2 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_last)
# summary(reg2)
# reg3 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_rollavg)
# summary(reg3)
# #regression with average of first and last entry in 10y skew as a predictor
# reg4 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_avg)
# summary(reg4)
# reg5 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_log_avg)
# summary(reg5)
# 
# logreg <- glm(!skew_df$Var_best ~ skew_df$skew_avg,family=binomial(link="logit"))
# summary(logreg)
# 
# ggplot(skew_df,aes(Var_best,skew_avg))+geom_boxplot()+
#   ggtitle("RMSE Variance < RMSE Semivariance as a function of skewness")
# # boxplot(skew_df$skew_avg~skew_df$Var_best)
# # title("RMSE Variance < RMSE Semivariance as a function of skewness")
# 
# #if data contains bootstrap columns show last period stats
# if (!is.null(dim(bootcols))) {
#   for (i in seq_along(stocknames)) {
#     print(stocknames[i])
#     print(StocksList[[i]][N,bootcols])
#   }
# }

# for (i in length(StocksList)) {
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
