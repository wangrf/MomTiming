
source(file.path(substr(getwd(),1,22),"header.R"))

ifupdate=F
mydate="2019-5-10"


mypath.R="./R/"
mypath.data="./data/"
sapply(paste0(mypath.R,dir(mypath.R)),source)

load(paste0(mypath.data,dir(path=mypath.data,pattern="(mom)*(RData)")))

if(ifupdate){
  library(WindR)
  w.start()
  get.mom(startDate = "2000-1-4",endDate = mydate,dataPath=dataPath)
}


equity.syms=substr(dir(dataPath,"CI"),1,11)
#equity.syms="000001.SH"
cash.sym="003003.OF"
method = "both"
#method="both"
benchmark = "000001.SH"
endDate=mydate

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
ttspan<-which(index(x)==as.Date(mydate,"%Y-%m-%d"))
idx.topN<-match.names("rankINtopN",names(get(equity.syms[1])))
idx.ratio<-match.names("entryRatio",names(get(equity.syms[1])))
y<-t(sapply(equity.syms,function(x) unlist(get(x)[ttspan,c(idx.topN,idx.ratio)])))
y<-y[y[,1]==1,]
y
loadRData("secName")
rownames(y)<-secName[match(rownames(y),secName$code),2]
print(y)
mypath=paste0(upPath,"0000-00-05Production/momTiming/")
file.remove(paste0(mypath,    dir(path = mypath,pattern = ".RData")))
save.image(paste0(mypath,"momProduct",mydate,".RData"))
