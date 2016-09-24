library(ggplot2)
library(reshape2)
library(gridExtra)
library(utils)
library(quantmod)

#constant defining how many months of history to use, 120 for 10y
n_window <- 120
run_bootstrap=FALSE

# Load data from disk: stock, LIBOR interest rate and SP500 global index
load("../DataWork/Stocks.Rdata")
load("../DataWork/LIBOR.Rdata")
load("../DataWork/SP500.Rdata")
load("../DataWork/cap_pct.Rdata")

N <- dim(LIBOR)[1] #number of periods assumed consistent for all data structures

#################### Data structures #############################
# Stocks, LIBOR, SP500 xts objects with prices for the 20y period #
# dat - merged xts object of Stocks, LIBOR, SP500                 #
# dat.ret - monthly return xts object - used for plotting        #
# DistMat - distribution matrix of 10y stock+index returns       #
#           used as probability proxy for each stock             #
#           from current price create stock expected prices dist #
#           and for each PriceGuess generate stock returns for   #
#           use in RTR reward to risk maximization               #
# StocksList - list of xts objects for each stock with actual    #
#              monthly price and estimated prices for each risk  #
#              discount factor                                   #
##################################################################

LIBOR <- LIBOR/12/100 #transform to monthly percent
dat <- merge(LIBOR,SP500,Stocks)

norm_col <- function(x) x/x[1]  #divide by first entry, oldest
#removing Apple from plot, 1000% stock return in 20y
rm_cols <- which(colnames(dat) %in% c("LIBOR","AAPL"))
plotdat <- as.data.frame(apply(dat[,-rm_cols],2,norm_col))
plotdat.xts <- apply(dat[,-rm_cols],2,norm_col)
plotdat$Date <- index(dat)
plotdat <- melt(plotdat,id="Date",value.name = "NormalizedPrice")
ggplot(plotdat,aes(x=Date,y=NormalizedPrice,colour=variable))+geom_line()+
  ggtitle("SP500 vs Stocks scaled returns")

#graph with index,ibm and libor each in a separate row
plotdat2 <- data.frame(Date=index(dat),coredata(dat))
g1 <- ggplot(plotdat2,aes(x=Date,y=IBM))+geom_line()+labs(x="Date",y="Price")+
  ggtitle("IBM stock price")
g2 <- ggplot(plotdat2,aes(x=Date,y=SP500))+geom_line()+labs(x="Date",y="Price")+
  ggtitle("SP500 index price")
g3 <- ggplot(plotdat2,aes(x=Date,y=LIBOR))+geom_line()+labs(x="Date",y="Rate")+
  ggtitle("LIBOR rate")
grid.arrange(g1,g2,g3,nrow=3)

dat.ret <- ROC(dat,type="discrete",na.pad=F) #returns calculation

ggplot(data.frame(coredata(dat.ret$SP500),coredata(dat.ret$IBM)))+
  geom_density(aes(x=SP500),fill="grey",alpha=0.5)+
  geom_density(aes(x=IBM),fill="blue",alpha=0.2) +
  ggtitle("Returns distributions")

ggplot(data.frame(coredata(dat.ret$SP500),coredata(dat.ret$IBM)),
       aes(x=SP500,y=IBM))+geom_point()+ggtitle("Returns distributions")

colnames(LIBOR) <- "Rate"
colnames(SP500)  <- "Price"
SP500$Return <- ROC(SP500$Price,type="discrete",na.pad=F) #Rate of change, return %
SP500$AvgRet <- rollapply(SP500$Return,FUN=mean,width=n_window)
SP500$SD     <- rollapply(SP500$Return,FUN=sd,  width=n_window)

stocknames <- colnames(Stocks)
#create list of stocks where each xts object has price,return,sd...
StocksList <- vector("list",length(stocknames))
names(StocksList) <- stocknames
for (i in seq_along(stocknames)) {
  StocksList[[i]] <- Stocks[,i]
  names(StocksList[[i]]) <- "Price"
  #arithmetic return (Rate of Change) calculation for each month
  StocksList[[i]]$Return <- ROC(StocksList[[i]]$Price,type="discrete",na.pad=F)
  #moving average (rolling) for n_window back history
  StocksList[[i]]$AvgRet <- rollapply(StocksList[[i]]$Return,FUN=mean,width=n_window)
  #standard deviation (cumulative rolling) for n_window back history
  StocksList[[i]]$SD <- rollapply(StocksList[[i]]$Return,FUN=sd,width=n_window)
  #Next period price predict based on average return known
  StocksList[[i]]$PredictNextPrice <- StocksList[[i]]$Price*(1+StocksList[[i]]$AvgRet)
  #Price taking into account CAPM risk
  cov_stock_index <- runCov(StocksList[[i]]$Return,SP500$Return,n=n_window)
  #alternative cov calculation using rollapply
  #rollapply(merge(StocksList[[i]]$Return,SP500$Return),
  #           FUN=function(x) {cov(x[,1],x[,2])},width=n_window,by.column = F)
  StocksList[[i]]$Beta <- cov_stock_index/(StocksList[[i]]$SD^2)
  CAPM_discount <- 1+LIBOR$Rate+StocksList[[i]]$Beta*(SP500$AvgRet-LIBOR$Rate)
  StocksList[[i]]$CAPMPrice <- StocksList[[i]]$PredictNextPrice/CAPM_discount
}

#graph showing CAPM prediction for return of stock vs market and risk free rate
sdplot <- 0 ; retplot <- coredata(last(LIBOR$Rate))
for (i in seq_along(stocknames)) {
  sdplot <- c(sdplot,as.numeric(last(StocksList[[i]]$Beta)))
  retplot <- c(retplot,as.numeric(last(StocksList[[i]]$AvgRet)))
}

plot(c(sdplot,1),c(retplot,last(SP500$AvgRet)), #Index beta=1 by definition
     xlab="Beta",ylab="Mean Monthly Return",
     ylim=c(-0.012,0.03),
     xlim=c(0,1.2),
     main="Stocks on SML - Security market line")
text(c(sdplot,1),c(retplot,last(SP500$AvgRet)), #Index beta=1 by definition
     c("Rf",stocknames,"SP500"),pos=3,cex=0.6)
abline(as.numeric(last(LIBOR$Rate)),
       as.numeric(last(SP500$AvgRet)-last(LIBOR$Rate)),
       col="blue",lty="dotted")

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

#calculate reward to risk ratio of stock+index portfolio for different risk functions
#from assumed probability distribution of returns DistMat and assumed price PriceGuess
RTR <- function(StockWeight,PriceGuess,RiskFunc,Rf,DistMat){
  DistMat$Ret_i <- DistMat$StockPrices/PriceGuess-1  #derived returns
  #Portfolio returns vector
  RetVec <- StockWeight*DistMat$Ret_i+(1-StockWeight)*DistMat$IndexRet
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
RTRpctmax <- function(PriceGuess,RiskFunc,Rf,DistMat) {
  pctmax <- optimize(RTR,c(0,1),PriceGuess=PriceGuess,            #optimize stock% between 0-1
                     RiskFunc=RiskFunc,Rf=Rf,DistMat=DistMat,
                     maximum=TRUE,tol=1e-9)$maximum               #find max
  return(pctmax)
}

# cap_pct <- 0.000001 #percent of capitalization of stock out of total market index
# #define convex function that has 0 at the lowest non-zero stock pct resolution
# f <- function(x,...) {RTRpctmax(x,...)-cap_pct}

#Start calculations from first entry that incorporates full window used, no N/As
calc_start <- N-n_window+1
#stocknames <- stocknames[1] #for debug only first stock
Rprof("../DataWork/Profiling.out",line.profiling = T)
#vartime <- system.time(
  for (i in seq_along(stocknames)) {
    StocksList[[i]]$VarPrice     <-NA
    StocksList[[i]]$SVarPrice    <-NA
    StocksList[[i]]$VAR5pctPrice <-NA
    if (run_bootstrap==TRUE) {
      StocksList[[i]]$BootVar      <-NA
      StocksList[[i]]$BootVarCI5   <-NA
      StocksList[[i]]$BootVarCI95  <-NA
      StocksList[[i]]$BootSVar     <-NA
      StocksList[[i]]$BootSVarCI5  <-NA
      StocksList[[i]]$BootSVarCI95 <-NA
      StocksList[[i]]$BootVAR      <-NA
      StocksList[[i]]$BootVARCI5   <-NA
      StocksList[[i]]$BootVARCI95  <-NA
    }
    #define convex function that has 0 at the lowest non-zero stock pct resolution
    f <- function(x,...) {RTRpctmax(x,...)-cap_pct[i]}
#    pb <- txtProgressBar(min=calc_start,max=N,style=3)
    #1.5h current run time without bootstrap, on i5 PC 0.6h with boot 100
    DistMatTotal <- merge(SP500$Return,StocksList[[i]]$Return) #distribution matrix
    names(DistMatTotal) <- c("IndexRet","StockRet")
    for (j in calc_start:N) {
    #  for (j in N:N) {  #for testing only last period
#      setTxtProgressBar(pb,j)
      rf <- as.numeric(LIBOR$Rate[j])
      cpr <- as.numeric(StocksList[[i]][j,"Price"])                #current price abbreviation
      DistMat <- DistMatTotal[(j-n_window+1):j]
      DistMat$StockPrices <- cpr*(1+DistMat$StockRet)  #distribution of expected prices
      DistMat <- DistMat[,c("IndexRet","StockPrices")]
      StocksList[[i]][j,"VarPrice"]     <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="pop.sd",
                                                   Rf=rf,DistMat=DistMat)$root
      StocksList[[i]][j,"SVarPrice"]    <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="svar",
                                                   Rf=rf,DistMat=DistMat)$root
      StocksList[[i]][j,"VAR5pctPrice"] <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="VAR5pct",
                                                   Rf=rf,DistMat=DistMat)$root
      if ((j==N) && (run_bootstrap==TRUE)) {
        bootN <- 10000 #run time 7.7hours on i5 PC for 10000
        VarPrice_vec  <- rep(0,bootN)
        SVarPrice_vec <- rep(0,bootN)
        VARPrice_vec  <- rep(0,bootN)
        for (q in 1:bootN) {
          DistMatSample <- DistMat[sample(n_window,replace=T)]        #bootstrap samples
          VarPrice_vec[q] <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="pop.sd",
                                  Rf=rf,DistMat=DistMatSample)$root
          SVarPrice_vec[q] <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="svar",
                                     Rf=rf,DistMat=DistMatSample)$root
          VARPrice_vec[q] <- uniroot(f,c(cpr/2,cpr*2),RiskFunc="VAR5pct",
                                                                                                 Rf=rf,DistMat=DistMatSample)$root
        }
        hist(VarPrice_vec)
        StocksList[[i]][j,"BootVar"] <- mean(VarPrice_vec)
        StocksList[[i]][j,"BootVarCI5"] <- quantile(VarPrice_vec,0.05)
        StocksList[[i]][j,"BootVarCI95"] <- quantile(VarPrice_vec,0.95)
        hist(SVarPrice_vec)
        StocksList[[i]][j,"BootSVar"] <- mean(SVarPrice_vec)
        StocksList[[i]][j,"BootSVarCI5"] <- quantile(SVarPrice_vec,0.05)
        StocksList[[i]][j,"BootSVarCI95"] <- quantile(SVarPrice_vec,0.95)
        hist(VARPrice_vec)
        StocksList[[i]][j,"BootVAR"] <- mean(VARPrice_vec)
        StocksList[[i]][j,"BootVARCI5"] <- quantile(VARPrice_vec,0.05)
        StocksList[[i]][j,"BootVARCI95"] <- quantile(VARPrice_vec,0.95)
      }
    }
#    close(pb)
  }
#  ,gcFirst=T)

Rprof(NULL)

save(StocksList,file="../DataWork/StocksList.Rdata")

# TBD add regression between history average return and predicted return for different stocks and check R2

for (i in seq_along(stocknames)) {
  #sum of squares of the error for variance risk predict
  RMSE1 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-CAPMPrice)/Price)^2)))
  RMSE2 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-VarPrice)/Price)^2)))
  RMSE3 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-SVarPrice)/Price)^2)))
  RMSE4 <- with(StocksList[[i]][calc_start:N],sqrt(mean(((Price-VAR5pctPrice)/Price)^2)))
  cat(sprintf("%-4s RMSE: CAPM %.4f, Variance %.4f, Semivariance %.4f, VAR5pct %.4f \n",
              stocknames[i],RMSE1,RMSE2,RMSE3,RMSE4))
}

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