
momData.fun <- function(method = c("marketTiming", "assetRotaion", "both"),
                        equity.syms = c("000300.SH", "000905.SH"),
                        topN = ifelse(method!="marketTiming",min(5,ceiling(length(equity.syms)/5)),1),
                        maxStockLevel=1,
                        cash.sym = "003003.OF",
                        startDate = "2001-01-04",
                        endDate = "2018-09-10",
                        daysInter = c("dayevery", "monfirst", "monlast", "weekfirst", "weeklast"),
                        industryInter = c("weekfirst", "monfirst", "monlast", "weeklast", "dayevery"),
                        holdIdx = c("min", "avg", "max"),
                        dataPath = paste0(paste0(
                          substr(getwd(), 1, max(unlist(
                            gregexpr("/", mypath, useBytes = T)
                          )))),"0000-00-02Data"),
                        initEq = 100000000,
                        k.turnEMA = 5,
                        k.turnRank = 250,
                        k.priceMA = 3,
                        k.priceRank = 100,
                        k.priceEMA = 5,
                        k.priceERank = 250,
                        k.ratio = 3,
                        k.priceInter = 20,
                        k.rollRate = 60,
                        k.pastDay = 30,
                        perct =
                          c(0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15, 0.05, 0, -0.01),
                        posLevel =
                          c(0.95, 0.85, 0.70, 0.55, 0.40, 0.25, 0.15, 0.10, 0.05),
                        posLevel.turn =
                          c(1, 1, 1, 1, 1, 1, 0.1, 0.08, 0.05),
                        posLevel.rate =
                          c(0.95, 0.85, 0.70, 0.55, 0.40, 0.25, 0.15, 0.10, 0.05)) {
  
  require(xts)
  require(TTR)
  require(PerformanceAnalytics)
  
  print(paste0("Set the max stock num to  ",topN))
  print("ss")
  na.method <- pmatch(method, c("marketTiming", "assetRotation", "both"))
  if (is.na(na.method)) {
    stop("invalid 'method' argument")
  }
  
  daysInter = match.arg(daysInter)
  industryInter = match.arg(industryInter)
  holdIdx = match.arg(holdIdx)
  
  print("e")
  if (method == "marketTiming") {
    if(topN > 1) print(paste0("topN larger than 1, the maximum stock ratio will be set to ",1/topN))
    if (length(equity.syms) > 1) {
      stop("length of equity.syms for marketTiming method larger than one")
    }
    
  }
  if (method == "assetRotation" |method == "both" ) {
    if (length(equity.syms) < 2) {
      stop("length of equity.syms for assetRotation method larger than two")
    }
    
    if (length(equity.syms) < topN) {
      stop("topN is larger than the length of equity.syms")
    }
    
  }
  print("k")
  tradeStart <-
    max(k.turnEMA,
        k.turnRank,
        k.priceEMA,
        k.priceRank,
        k.priceMA,
        k.priceERank) + 1
  
  acct.syms <- c(equity.syms,cash.sym)
  print("c")
  
  
  for(sym in acct.syms){
    load(paste0(dataPath,"/",sym,".RData"),envir = .GlobalEnv)
  }
  print("a")
  ## cash Fund for trading
   loadRData(cash.sym)
   xx<-get(cash.sym,envir=.GlobalEnv)
  xx<-xts(data.frame(open=xx[,1],high=xx[,1],low=xx[,1],close=xx[,1]),order.by=index(xx))
  names(xx) <- paste0(cash.sym,".",c("open","high","low","close"))
  assign(cash.sym,xx,envir = .GlobalEnv)
  
  ## aligns rawData for the same timeSpan
  alignSymbols(acct.syms)

  x<-get(cash.sym,envir = .GlobalEnv)
  tSpan <- index(x)>=as.Date(startDate,"%Y-%m-%d")&index(x)<=as.Date(endDate,"%Y-%m-%d")
  for(sym in acct.syms){
    xx<-get(sym,envir = .GlobalEnv)
    xx<-xx[tSpan,]
    assign(sym,xx,envir = .GlobalEnv)
  }
  date.range=index(xx)
  print(paste0("Generating Data for mom Model with ",method," method in ",head(date.range,1)," to ",tail(date.range,1)))
  
  print(paste0("1. Adding indicators of mom"))
  for( i in 1:length(equity.syms)){
    
    x<-get(equity.syms[i],envir = .GlobalEnv)
    
    idx.num <- NCOL(x)
    
    idx.turn <- match.names("turn",colnames(x))
    idx.close <- match.names("close",colnames(x))
    
    
    r.mkt <- Return.calculate(x[,idx.close])
    rc.mkt <- rollapply(r.mkt,k.priceInter,Return.cumulative)
    roll.rate.rank <- runPercentRank(rc.mkt,n=k.rollRate)
    holdratio.rate <- posLevel.rate[apply(roll.rate.rank,1,function(x) which(x>=perct)[1])]
    
    
    turn.ema <- EMA(x[,idx.turn],n=k.turnEMA)
    turn.ema.rank <- runPercentRank(turn.ema,n=k.turnRank)
    holdratio.turn <- posLevel.turn[apply(turn.ema.rank,1,function(x) which(x>=perct)[1])]
    
    holdratio.turn=1 # test for no turn
    
    price.ma<-SMA(x[,idx.close],n=k.priceMA)
    price.ma.rank <- runPercentRank(price.ma,n=k.priceRank)
    holdratio.price <- posLevel[apply(price.ma.rank,1,function(x) which(x>=perct)[1])]
    
    price.ema<-EMA(x[,idx.close],n=k.priceEMA)
    price.ema.rank <- runPercentRank(price.ema,n=k.priceERank)
    holdratio.priceE <- posLevel[apply(price.ema.rank,1,function(x) which(x>=perct)[1])]
    
    
    holdratio.avg <- rowMeans(cbind(holdratio.rate,holdratio.turn,holdratio.price,holdratio.priceE),na.rm=T)
    
    holdratio.max <-
      apply(cbind(holdratio.rate,holdratio.turn,holdratio.price,holdratio.priceE),1,max,na.rm = T)
    
    holdratio.min <-
      apply(cbind(holdratio.rate,holdratio.turn,holdratio.price,holdratio.priceE),1,min,na.rm = T)
    
    
    
    a = .indexmon(x) + 1
    b = c(a[-1],13)
    f = c(1,diff(a))
    mon.last <- a != b
    mon.first <- f==1
    
    a = .indexweek(x)+1
    b=c(a[-1],8)
    f=c(1,diff(a))
    week.last <- a !=b
    week.first <- f==1
    
    day.every <- T
    
    
    x<-cbind(x,
             rc.mkt,roll.rate.rank,holdratio.rate,
             turn.ema,turn.ema.rank,holdratio.turn,
             price.ma,price.ma.rank,holdratio.price,
             price.ema,price.ema.rank,holdratio.priceE,
             holdratio.avg,holdratio.max,holdratio.min,
             mon.last,mon.first,week.last,week.first,day.every)
    
    
    
    colnames(x) <- c(colnames(x)[1:idx.num],
                     paste(equity.syms[i],c("roll.rate","roll.rate.rank","holdratio.rate",
                                            "turn.ema","turn.ema.rank","holdratio.turn","price.ma",
                                            "price.ma.rank","holdratio.price","price.ema","price.ema.rank",
                                            "holdratio.priceE","holdratio.avg","holdratio.max","holdratio.min",
                                            "mon.last","mon.first","week.last","week.first","day.every"),sep = "."))
    
    
    #   
    #   x[1,] <- c(rep(1,4),rep(0,NCOL(x)-6),1,0)
    #   x[2,] <- c(rep(1,4),rep(0,NCOL(x)-6),0,1)
    #   
    assign(equity.syms[i],x,envir = .GlobalEnv)
    
  }
  
  print(paste0("2. Adding Signals and holdRatio"))
  
  for(i in 1:length(equity.syms)){
    
    mktdata <- get(equity.syms[i],envir = .GlobalEnv)
    
    idx.monlast = match.names("mon.last",names(mktdata))
    idx.monfirst = match.names("mon.first",names(mktdata))
    idx.weeklast = match.names("week.last",names(mktdata))
    idx.weekfirst = match.names("week.first",names(mktdata))
    idx.dayevery = match.names("day.every",names(mktdata))
    
    idx.holdturn = match.names("holdratio.turn",names(mktdata))
    idx.holdprice = match.names("holdratio.price",names(mktdata))
    idx.holdpriceE = match.names("holdratio.priceE",names(mktdata))
    idx.holdmin = match.names("holdratio.min",names(mktdata))
    idx.holdavg = match.names("holdratio.max",names(mktdata))
    idx.holdmax = match.names("holdratio.avg",names(mktdata))
    
    entrySig <- (switch(daysInter,
                        monlast = mktdata[,idx.monlast],
                        monfirst = mktdata[,idx.monfirst],
                        weeklast = mktdata[,idx.weeklast],
                        weekfirst = mktdata[,idx.weekfirst],
                        dayevery = mktdata[,idx.dayevery]))
    entrySig[1:(tradeStart-1)] = 0
    
    
    names(entrySig) <- paste(equity.syms[i],"entrySig",sep=".")
    
    daynum <- (switch(daysInter,
                      monlast = 20,
                      monfirst = 20,
                      weeklast = 7,
                      weekfirst = 7,
                      dayevery = 1))
    
    
    
    entryRatio <- switch(holdIdx,
                         turn = mktdata[,idx.holdturn],
                         price = mktdata[,idx.holdprice],
                         priceE = mktdata[,idx.holdprice],
                         min = mktdata[,idx.holdmin],
                         max = mktdata[,idx.holdmax],
                         avg = mktdata[,idx.holdavg])
    entryRatio[1:(tradeStart-1)] = 0
    
    # entryRatio.MA <-SMA(entryRatio,n = trunc(k.ratio),na.rm=T)
    # entryRatio.lag <- c(entryRatio[1],entryRatio[-nrow(entryRatio)])
    
    # entryRatio <- ifelse(entryRatio.MA<entryRatio,entryRatio,entryRatio.lag/2)
    
    entryRatio <- c(entryRatio[1],entryRatio[-nrow(entryRatio)])
    
    if(method == "assetRotation"){
      entryRatio = 1
    }
    
    names(entryRatio) <- paste(equity.syms[i],"entryRatio",sep=".")
    
    entryRatio<-ifelse(entrySig==1,entryRatio,0)
    
    
    entryRatio <- xts(as.vector(entryRatio)/topN*maxStockLevel,order.by=index(mktdata))
    entrySig <- xts(as.vector(entrySig),order.by=index(mktdata))
    
    temp<-names(mktdata)
    mktdata <- cbind(mktdata,entrySig,entryRatio)
    
    names(mktdata) <- c(temp,paste(equity.syms[i],c("entrySig","entryRatio"),sep="."))
    
    assign(equity.syms[i],mktdata,envir = .GlobalEnv)
    
    
  }
  print(paste0("3. Calculating rollReturns for each sym"))
  
  for(i in 1:length(equity.syms)){
    
    mktdata <- get(equity.syms[i],envir = .GlobalEnv)
    
    idx.close <- match.names("close",names(mktdata))
    r.mkt <- Return.calculate(mktdata[,idx.close])
    rc.mkt <- rollapply(r.mkt,k.pastDay,Return.cumulative)
    
    rc.mkt <- c(rc.mkt[1],rc.mkt[-nrow(rc.mkt)])
    rc.mkt <- xts(as.vector(rc.mkt),order.by = index(mktdata))
    
    temp<-names(mktdata)
    mktdata <- cbind(mktdata,rc.mkt)
    names(mktdata) <- c(temp,paste(equity.syms[i],c("nPastReturn"),sep="."))
    
    assign(equity.syms[i],mktdata,envir = .GlobalEnv)
    
  }
  
  if(method!="marketTiming"){
    print(paste0("3.1. Creating syms list for rotation "))
    symbols.nPastReturn<-merge_ind(equity.syms,ind="nPastReturn")
    nPastReturn.rank <- applyRank(x=symbols.nPastReturn,rankFun=rowRank)
    names(nPastReturn.rank) <- gsub(".nPastReturn",".nPastReturnRank",names(nPastReturn.rank))
    stopifnot(all.equal(gsub("(X)|(.nPastReturnRank)","",colnames(nPastReturn.rank)),equity.syms))
    
    for(i in 1:length(equity.syms)){
      
      mktdata <- get(equity.syms[i],envir = .GlobalEnv)
      idx.entryRatio <- match.names("entryRatio",names(mktdata))
      
      pr<-nPastReturn.rank[,i]
      pr<-xts(pr,order.by=index(mktdata))
      industrySig <- (switch(industryInter,
                             monlast = mktdata[,idx.monlast],
                             monfirst = mktdata[,idx.monfirst],
                             weeklast = mktdata[,idx.weeklast],
                             weekfirst = mktdata[,idx.weekfirst],
                             dayevery = mktdata[,idx.dayevery]))
      industrySig<-xts(industrySig,order.by=index(mktdata))
      rank.id <- which(industrySig==1)
      pr.topN<-rep(c(pr[rank.id]),diff(c(rank.id,nrow(mktdata)+1))) <= topN
      pr.topN <- xts(pr.topN,order.by=index(mktdata))
      
      
      temp<-names(mktdata)
      mktdata <- cbind(mktdata,pr,industrySig,pr.topN)
      names(mktdata) <- c(temp,paste(equity.syms[i],c("nPastReturnRank","industrySig","rankINtopN"),sep="."))
      
      mktdata[,idx.entryRatio] <- mktdata[,idx.entryRatio] * pr.topN
      assign(equity.syms[i],mktdata,envir = .GlobalEnv)
      
    }
  }
  
  print(paste0("4. Creating Data for the cash.sym"))
  
  symbols.entryRatio <- merge_ind(symbols = equity.syms,ind = "entryRatio")
  entryRatio <- 1- rowSums(symbols.entryRatio)
  
  x <- get(cash.sym,envir = .GlobalEnv)
  idx.num <- NCOL(x)
  
  a = .indexmon(x) + 1
  b = c(a[-1],13)
  f = c(1,diff(a))
  mon.last <- a != b
  mon.first <- f==1
  
  a = .indexweek(x)+1
  b=c(a[-1],8)
  f=c(1,diff(a))
  week.last <- a !=b
  week.first <- f==1
  
  day.every <- T
  
  entrySig <- (switch(daysInter,
                      monlast = mktdata[,idx.monlast],
                      monfirst = mktdata[,idx.monfirst],
                      weeklast = mktdata[,idx.weeklast],
                      weekfirst = mktdata[,idx.weekfirst],
                      dayevery = mktdata[,idx.dayevery]))
  entrySig[1:(tradeStart-1)] = 0
  
  entrySig <- xts(as.vector(entrySig),order.by=index(mktdata))
  
  x<-cbind(x,
           mon.last,mon.first,week.last,week.first,day.every,entrySig,entryRatio)
  
  
  colnames(x) <- c(colnames(x)[1:idx.num],
                   paste(cash.sym,c("mon.last","mon.first","week.last","week.first","day.every","entrySig","entryRatio"),sep = "."))
  
  assign(cash.sym,x,envir = .GlobalEnv)
  
  print("5. restore parameters for mom")
  momPara <- list(
    method = method,
    equity.syms = equity.syms,
    topN = topN,
    cash.sym = cash.sym,
    acct.syms = acct.syms,
    daysInter = daysInter,
    industryInter = industryInter,
    holdIdx = holdIdx,
    dataPath = dataPath,
    tradeStart=tradeStart,
    initEq = initEq,
    k.turnEMA = k.turnEMA,
    k.turnRank = k.turnRank,
    k.priceMA =  k.priceMA,
    k.priceRank = k.priceRank,
    k.priceEMA = k.priceEMA,
    k.priceERank = k.priceERank,
    k.ratio = k.ratio,
    k.priceInter = k.priceInter,
    k.rollRate = k.rollRate,
    k.pastDay =  k.pastDay,
    perct =
      perct,
    posLevel =
      posLevel,
    posLevel.turn =
      posLevel.turn ,
    posLevel.rate =
      posLevel.rate
  )
  assign("momPara",momPara,envir = .GlobalEnv)
  print("End")

}
