momPlot.fun <- function(benchmark = "000300.SH"){
  
  initEq <- get("momPara",envir=.GlobalEnv)$initEq
  tradeStart <-  get("momPara",envir = .GlobalEnv)$tradeStart
  method <- get("momPara",envir = .GlobalEnv)$method
  
  
  strategy<-(portf$summary$total/initEq)[-(1:(tradeStart-1))]
  names(strategy)<- paste0(method," strategy")
  ratio<-(portf$summary$holdratio)[-(1:tradeStart)]
  names(ratio)<-"Stock Position Level"
  strategy<-cbind(strategy,ratio)
  
  
 loadRData(benchmark)
  xx<-get(benchmark)
  if(benchmark=="003003.OF"){
    
    xx<-xts(data.frame(open=xx[,1],high=xx[,1],low=xx[,1],close=xx[,1]),order.by=index(xx))
    names(xx) <- paste0(benchmark,".",c("open","high","low","close"))
  }
  x<-xx
  
  idx.close <- match.names("close",names(x))
  y<-x[index(strategy),idx.close]
  y<-round(y/as.numeric(y[1]),4)
  names(y)<- paste0("benchmark:",benchmark)
  
  stgy.yield<-merge(strategy,y,join = "inner")
  print(plot.xts(stgy.yield,main = "stategy Performace Vs benchmark",legend.loc="topleft"))
  
  stgy.yield
  
  
}