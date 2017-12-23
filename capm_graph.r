library(ggplot2)

#return and standard deviation of stock and market
ri <- 0.025
rm <- 0.015
si <- 0.8
sm <- 0.4
covim <- 0.2

w <- -50:100/100
rp <- w*ri+(1-w)*rm
sp <- (((w*si)^2+((1-w)*sm)^2+2*covim*w*(1-w)))^0.5
g1 <- ggplot(data.frame(x=sp,y=rp),aes(x,y))+geom_path()
g1 <- g1 + geom_segment(x=0,y=0.005,xend=1,yend=0.033) +
  coord_cartesian(xlim=c(0,1.1),ylim=c(0,0.05),expand=T)
g1 <- g1+geom_segment(x=0,y=0,xend=0,yend=0.05,
                      arrow=arrow(ends="last",type="closed",
                                  angle=15,length=unit(3,"mm")))
g1 <- g1+geom_segment(x=0,y=0,xend=1.1,yend=0,
                      arrow=arrow(ends="last",type="closed",
                                  angle=15,length=unit(3,"mm")))
g1 <- g1+theme_classic()+xlab("stdv")+ylab("return")
g1 <- g1 + geom_text(x=-0.002,y=0.005,label="rf",hjust="right",size = 3)
g1
