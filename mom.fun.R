rm(list=ls(),envir = .GlobalEnv)


library(xts)
library(PerformanceAnalytics)
# load Toolbox and data
# set initial Vlues
Sys.setenv(TZ="UTC")
mypath <- getwd()
upPath <- paste0(
  substr(mypath, 1, max(unlist(
    gregexpr("/", mypath, useBytes = T)
  ))))
toolPath <- paste0(upPath,"0000-00-01ToolBox")
dataPath <- paste0(upPath,"0000-00-02Data")
sapply(paste0(toolPath,"/",dir(toolPath,pattern = ".R")),source)

equity.syms=substr(dir(dataPath,"CI"),1,11)
#equity.syms="000001.SH"
cash.sym="003003.OF"
#method = "marketTiming"
method="both"
benchmark="000001.SH"
endDate="2019-4-26"


sapply(equity.syms,loadRData)
loadRData("000001.SH")
momData.fun(method=method,
            equity.syms=equity.syms,
            cash.sym=cash.sym,
            startDate = "2005-01-04",
            endDate="2018-12-21",
            maxStockLevel = 1)

momTrade.fun(equity.syms=equity.syms,
             cash.sym=cash.sym)

momYield<-momPlot.fun(benchmark="000001.SH")
idx.mom <- match.names(method,names(momYield))
mom.pfms<-stgyPerformance(momYield[,idx.mom])
idx.benchmark <- match.names(benchmark,names(momYield))
benchmark.pfms<-stgyPerformance(momYield[,idx.benchmark])


momData.fun(method="marketTiming",equity.syms=equity.syms,cash.sym=cash.sym,endDate="2018-10-19")
momAddTrade.fun(equity.syms=equity.syms,cash.sym=cash.sym)
momYield<-momPlot.fun(benchmark="000001.SH")
idx.mom <- match.names(method,names(momYield))
mom.pfms<-stgyPerformance(momYield[,idx.mom])
idx.benchmark <- match.names(benchmark,names(momYield))
benchmark.pfms<-stgyPerformance(momYield[,idx.benchmark])


## update method of both for CI industry

equity.syms=substr(dir(dataPath,"CI"),1,11)
#equity.syms="000001.SH"
cash.sym="003003.OF"
method = "marketTiming"
#method="both"
benchmark = "000001.SH"
endDate="2018-10-19"

momData.fun(method=method,equity.syms=equity.syms,cash.sym=cash.sym,endDate=endDate,daysInter ="weeklast" )
momAddTrade.fun(equity.syms=equity.syms,cash.sym=cash.sym)
momYield<-momPlot.fun(benchmark=benchmark)
idx.mom <- match.names(method,names(momYield))
mom.pfms<-stgyPerformance(momYield[,idx.mom])
idx.benchmark <- match.names(benchmark,names(momYield))
benchmark.pfms<-stgyPerformance(momYield[,idx.benchmark])
#calculate avg positon for this week
idx.pos<-match.names("Position",names(momYield))
table.Weekly(momYield[,idx.pos],geometric = F)/table.Weekly(xts(data.frame(Stock.Position.Level=rep(1,nrow(momYield))),order.by=index(momYield)),geometric = F)
#get industry and its holdratio
x<-get(cash.sym)
ttspan<-which(index(x)==as.Date("2019-4-26","%Y-%m-%d"))
idx.topN<-match.names("rankINtopN",names(get(equity.syms[1])))
idx.ratio<-match.names("entryRatio",names(get(equity.syms[1])))
y<-t(sapply(equity.syms,function(x) unlist(get(x)[ttspan,c(idx.topN,idx.ratio)])))
y<-y[y[,1]==1,]
y
loadRData("secName")
rownames(y)<-secName[match(rownames(y),secName$code),2]
y


save(momYield,file=paste0(upPath,"2018-12-07择时监测/","momPosition.Rdata"))

mom<-momYield['2018/']
mom<-mom/matrix(c(as.numeric(mom[1,1]),1,as.numeric(mom[1,3])),nrow(mom),3,byrow=T)
tail(mom)
plot.xts(mom,main = "stategy Performace Vs benchmark",legend.loc="topright")










###-------------------------只大盘择???----------------------

rm(list=ls())
load("!")


library(xts)
library(PerformanceAnalytics)
# load Toolbox and data
# set initial Vlues
Sys.setenv(TZ="UTC")
mypath <- getwd()
upPath <- paste0(
  substr(mypath, 1, max(unlist(
    gregexpr("/", mypath, useBytes = T)
  ))))
toolPath <- paste0(upPath,"0000-00-01ToolBox")
dataPath <- paste0(upPath,"0000-00-02Data")
sapply(paste0(toolPath,"/",dir(toolPath,pattern = ".R")),source)


equity.syms="000001.SH"
cash.sym="003003.OF"
method = "marketTiming"
benchmark="000001.SH"
endDate="2019-2-22"



momData.fun(method=method,equity.syms=equity.syms,cash.sym=cash.sym,endDate=endDate,daysInter ="dayevery" )
momAddTrade.fun(equity.syms=equity.syms,cash.sym=cash.sym)
momYield<-momPlot.fun(benchmark=benchmark)
idx.mom <- match.names(method,names(momYield))
mom.pfms<-stgyPerformance(momYield[,idx.mom])
idx.benchmark <- match.names(benchmark,names(momYield))
benchmark.pfms<-stgyPerformance(momYield[,idx.benchmark])
#calculate avg positon for this week
idx.pos<-match.names("Position",names(momYield))
table.Weekly(momYield[,idx.pos],geometric = F)/table.Weekly(xts(data.frame(Stock.Position.Level=rep(1,nrow(momYield))),order.by=index(momYield)),geometric = F)
#get industry and its holdratio

save(momYield,file=paste0(upPath,"2018-12-07择时监测/","momPosition.Rdata"))


