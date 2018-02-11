#analyze previous runs

library(ggplot2)
library(quantmod)
library(reshape2)
library(gridExtra)
library(moments)    #for skewness function

startDate <- "1996-1-1"
midDate   <- "2005-12-31"
midDate_1 <- "2006-1-1"
endDate   <- "2015-12-31"

if (dir.exists("C:/Users/Claudiu/Dropbox")) {
  droppath <- "C:/Users/Claudiu/Dropbox" #Dell laptop
} else {
  droppath <- "D:/Claudiu/Dropbox"       #Home PC
}

#historical use for bootstrap
#load(file="C:/Users/Claudiu/Dropbox/Thesis/Docs/Data/StocksList26092016Daily_5stocks.Rdata")
#load(file="C:/Users/Claudiu/Dropbox/Thesis/Docs/Data/StocksList21092016Bootstrap10000_5stocks.Rdata")
#load(file="C:/Users/Claudiu/Dropbox/Thesis/Docs/Data/StocksList16122016Monthly48stocks.Rdata")

load(file=file.path(droppath,"Thesis/DataWork/StocksList_1_100_CPT_AVAR.Rdata"))
temp <- StocksList
load(file=file.path(droppath,"Thesis/DataWork/StocksList_101_200_CPT_AVAR.Rdata"))
StocksList <- c(temp,StocksList)
temp <- StocksList
load(file=file.path(droppath,"Thesis/DataWork/StocksList_201_400_CPT_AVAR.Rdata"))
StocksList <- c(temp,StocksList)
temp <- StocksList
load(file=file.path(droppath,"Thesis/DataWork/StocksList_401_600_CPT_AVAR.Rdata"))
StocksList <- c(temp,StocksList)
temp <- StocksList
load(file=file.path(droppath,"Thesis/DataWork/StocksList_601_778_CPT_AVAR.Rdata"))
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
StocksList <- StocksList[completeIndx]
StocksList <- StocksList[-417] #temp fix for TDS stock with stock split on 16/5/2005

for (i in 1:length(StocksList)) {
  #sum of squares of the error for variance risk predict
  RMSE1 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-CAPMPrice)/Price)^2)))
  RMSE2 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-VarPrice)/Price)^2)))
  RMSE3 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-SVarPrice)/Price)^2)))
  RMSE4 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-VAR5pctPrice)/Price)^2)))
  RMSE5 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-CPTPrice)/Price)^2)))
  RMSE6 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-AVARPrice)/Price)^2)))
  Err1 <- with(StocksList[[i]][calc_start:N],mean(((Price-VarPrice)/Price)))
  Err2 <- with(StocksList[[i]][calc_start:N],mean(((Price-SVarPrice)/Price)))
  cat(sprintf("%-4s RMSE: CAPM %.4f, Variance %.4f, Semivariance %.4f,
              VAR5pct %.4f, CPT %.4f, AVAR %.4f \n",
              stocknames[i],RMSE1,RMSE2,RMSE3,RMSE4,RMSE5,RMSE6))
  StocksList[[i]]$RMSE_Var <- RMSE2
  StocksList[[i]]$RMSE_SVar <- RMSE3
  StocksList[[i]]$RMSE_VAR5Pct <- RMSE4
  StocksList[[i]]$RMSE_CPT <- RMSE5
  StocksList[[i]]$RMSE_AVAR <- RMSE6
  StocksList[[i]]$Err_Var <- Err1
  StocksList[[i]]$Err_SVar <- Err2
  StocksList[[i]]$skewavg <- mean(StocksList[[i]]$Skew,na.rm=T)
  StocksList[[i]]$LogReturn <- ROC(StocksList[[i]]$Price,type="continuous",na.pad=F)
  #skewness (cumulative rolling) for n_window back history
  StocksList[[i]]$LogSkew <- rollapply(StocksList[[i]]$LogReturn,FUN=skewness,
                                    width=n_window,na.rm=T)
}



#descriptive statistics for 2 periods, average for all stocks for all months
mean_func <- function(x,a,b) {return(mean(coredata(x[paste0(a,"/",b)]$Return),na.rm=T))}
sd_func   <- function(x,a,b) {return(sd(coredata(x[paste0(a,"/",b)]$Return),na.rm=T))}
skew_func <- function(x,a,b) {return(skewness(coredata(x[paste0(a,"/",b)]$Return),na.rm=T))}
StocksStat1 <- rbind(mean(sapply(StocksList, mean_func,startDate,midDate)),
                    mean(sapply(StocksList, sd_func,   startDate,midDate)),
                    mean(sapply(StocksList, skew_func, startDate,midDate)))
StocksStat2 <- rbind(mean(sapply(StocksList, mean_func,midDate_1,endDate)),
                     mean(sapply(StocksList, sd_func,  midDate_1,endDate)),
                     mean(sapply(StocksList, skew_func,midDate_1,endDate)))
StocksStat <- cbind(StocksStat1,StocksStat2)
row.names(StocksStat) <- c("Mean","Std Dev","Skewness")
colnames(StocksStat)  <- c("1995-2005","2006-2015")
print(StocksStat)

#descriptive statistics - histogram for monthly cross-section of stocks returns
statret <- sapply(StocksList,function(x) {return(coredata(x$Return))})
stat_df1 <- data.frame(Dates=index(StocksList[[1]]),
                      Mean=apply(statret,1,mean))
stat_df1 <- stat_df1[-1,]  #remove first NA row
ggplot(stat_df1,aes(Mean))+geom_histogram(binwidth=0.03)
#histograms for std dev and skewness
stat_df2 <- data.frame(StdDev=apply(statret,2,sd,na.rm=T),
                       Skewness=apply(statret,2,skewness,na.rm=T))
ggplot(stat_df2,aes(StdDev))+geom_histogram(binwidth=0.02)
ggplot(stat_df2,aes(Skewness))+geom_histogram(binwidth=0.3)


#extract vector of RMSE %
RMSE_Var      <- sapply(lapply(StocksList,last),function(x) {return(x$RMSE_Var)})
RMSE_SVar     <- sapply(lapply(StocksList,last),function(x) {return(x$RMSE_SVar)})
RMSE_VAR5Pct  <- sapply(lapply(StocksList,last),function(x) {return(x$RMSE_VAR5Pct)})
RMSE_CPT      <- sapply(lapply(StocksList,last),function(x) {return(x$RMSE_CPT)})
RMSE_AVAR     <- sapply(lapply(StocksList,last),function(x) {return(x$RMSE_AVAR)})
Err_Var     <- sapply(lapply(StocksList,last),function(x) {return(x$Err_Var)})
Err_SVar     <- sapply(lapply(StocksList,last),function(x) {return(x$Err_SVar)})
#descriptive statistics in %
RMSE_summary <- 100*rbind(summary(RMSE_Var),summary(RMSE_SVar),summary(RMSE_VAR5Pct),
                          summary(RMSE_CPT),summary(RMSE_AVAR))
row.names(RMSE_summary)<-c("Variance","Semivariance","VAR","CPT","AVAR")
RMSE_summary

plot.df <- data.frame(stock=names(RMSE_Var),Variance=RMSE_Var,Semivariance=RMSE_SVar,
                      VaR=RMSE_VAR5Pct,CPT=RMSE_CPT,AVAR=RMSE_AVAR)
plot.df <- melt(plot.df,id="stock",value.name="RMSE",
                variable.name="Risk_Measure")
ggplot(plot.df,aes(Risk_Measure,RMSE))+geom_boxplot()

#boxplot(RMSE_Var,RMSE_SVar,RMSE_VAR5Pct,names=c("Variance","Semivariance","VAR"))

skew_last     <- sapply(lapply(StocksList,last),function(x) {return(x$Skew)})
skew_first    <- sapply(lapply(StocksList,function(x) {return(first(last(x,120)))}),
                        function(x) {return(x$Skew)})
skew_avg      <- 0.5*(skew_last+skew_first)
skew_rollavg  <- sapply(lapply(StocksList,last),function(x) {return(x$skewavg)})

skew_log_last     <- sapply(lapply(StocksList,last),function(x) {return(x$LogSkew)})
skew_log_first    <- sapply(lapply(StocksList,function(x) {return(first(last(x,120)))}),
                        function(x) {return(x$LogSkew)})
skew_log_avg      <- 0.5*(skew_log_last+skew_log_first)

skew_df <- as.data.frame(cbind(skew_last,skew_first,skew_avg,skew_rollavg,skew_log_avg,
                               RMSE_Var,RMSE_SVar,RMSE_CPT,RMSE_AVAR,Err_Var,Err_SVar))
skew_df$Var_best <- (skew_df$RMSE_Var < skew_df$RMSE_SVar)
#Labeling G1 group with lower variance RMSE and G2 for semivariance
skew_df$Predictor_Group <- ifelse((skew_df$RMSE_Var < skew_df$RMSE_SVar),
                                  "G1","G2")
ggplot(skew_df,aes(Predictor_Group,skew_avg))+geom_boxplot()
ggplot(skew_df,aes(skew_avg))+geom_density(aes(colour=Predictor_Group))

#  ggtitle("Stock returns skewness in optimal RMSE groups")
summary(skew_df$skew_avg)
summary(skew_df[skew_df$Predictor_Group=="G1",]$skew_avg)
summary(skew_df[skew_df$Predictor_Group=="G2",]$skew_avg)
table(skew_df$Predictor_Group)
skew_df$positive_skew <- ifelse(skew_df$skew_avg > 0.2,"Large","Small")
with(skew_df,table(Predictor_Group,positive_skew))

skew_df$beta <- sapply(lapply(StocksList,last),function(x) {return(x$Beta)})
table(skew_df$Var_best,skew_df$beta>1.5) 

reg1 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_first)
summary(reg1)
reg2 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_last)
summary(reg2)
reg3 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_rollavg)
summary(reg3)
#regression with average of first and last entry in 10y skew as a predictor
reg4 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_avg)
summary(reg4)
reg5 <- lm((skew_df$RMSE_Var>skew_df$RMSE_SVar) ~ skew_df$skew_log_avg)
summary(reg5)

logreg <- glm(!skew_df$Var_best ~ skew_df$skew_avg,family=binomial(link="logit"))
summary(logreg)

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

save(StocksList,file="../DataWork/StocksList_after_Analyze.Rdata")
