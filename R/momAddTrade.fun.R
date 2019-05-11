momAddTrade.fun<-function(equity.syms= c("000300.SH", "000905.SH"),
                          cash.sym="003003.OF"){
  
  acct.syms<-c(equity.syms,cash.sym)
  portf<-get("portf",envir = .GlobalEnv)
  xx<-get(cash.sym,envir = .GlobalEnv)
  if(tail(index(portf$summary),1)>=tail(index(xx),1)){
    stop("Update the momData first")
  }
  
  
  newid=nrow(portf$summary)+1
  
  date.range <- index(get(acct.syms[1]))
  print(paste0("Add trades from ",date.range[newid]," to ",tail(date.range,1)))
  
  for(i in 1:length(acct.syms)){
    
    temp <- c("qty","avgPrice","curPrice","realizedPos","unrealizedPos","txn","position","equity","holdratio")
    xx <- matrix(0,nrow=length(date.range)-newid+1,ncol=length(temp))
    colnames(xx) <- temp
    xx<- as.xts(xx,order.by=date.range[newid:length(date.range)])
    indexTZ(xx) <- "UTC"
    portf[[i]] <- rbind(portf[[i]],xx)
    
  }
  
  temp <- c("realizedPos","unrealizedPos","txn","position","equity","total","symNum","holdratio")
  xx <- matrix(0,nrow=length(date.range)-newid+1,ncol=length(temp))
  colnames(xx) <- temp
  xx<- as.xts(xx,order.by=date.range[newid:length(date.range)])
  indexTZ(xx) <- "UTC"
  portf[["summary"]] <- rbind(portf[["summary"]],xx)
  
  
  
  
  for(i in newid:length(date.range)){
    
    cur.date <- date.range[i]
    last.date <- date.range[i-1]
    print(data.frame(i,cur.date))
    ## position allocation
    
    trade.txn=0
    last.portfEq <- as.numeric(portf[["summary"]][last.date,"position"]) +as.numeric(portf[["summary"]][last.date,"equity"])
    
    
    for (sym in acct.syms) {
      
      
      mktdata <- get(sym,envir=.GlobalEnv)
      
      idx.open <- match.names("open",names(mktdata))
      idx.close <- match.names("close",names(mktdata))
      idx.high <- match.names("high",names(mktdata))
      idx.low <- match.names("low",names(mktdata))
      
      last.price <- as.numeric(mktdata[last.date,idx.close])
      last.avgPrice <- as.numeric(portf[[sym]][last.date,"avgPrice"])
      last.qty <- as.numeric(portf[[sym]][last.date,"qty"])
      last.pos <- as.numeric(portf[[sym]][last.date,"position"])
      last.equity <- as.numeric(portf[[sym]][last.date,"equity"])
      last.realizedPos <- as.numeric(portf[[sym]][last.date,"realizedPos"])
      last.txn <- as.numeric(portf[[sym]][last.date,"txn"])
      last.ratio <- as.numeric(portf[[sym]][last.date,"holdratio"])
      
      idx.ratio <- match.names("entryRatio",names(mktdata))
      cur.ratio <- as.numeric(mktdata[i,idx.ratio])
      cur.ratio <- ifelse(is.na(cur.ratio),0,cur.ratio)
      
      cur.highPrice <- as.numeric(mktdata[last.date,idx.high])
      
      idx.entrySig <- match.names("entrySig",names(mktdata))
      Sig <- as.numeric(mktdata[i,idx.entrySig])
      Sig<-ifelse(is.na(Sig),0,Sig)
      
      order.new = NULL
      
      if(Sig==1){
        
        maxPos <- trunc(last.portfEq * cur.ratio)
        maxQty <- trunc(maxPos / cur.highPrice / 100) * 100
        trade.qty <- maxQty - last.qty
        trade.price <-
          mean(mktdata[cur.date,idx.high],mktdata[cur.date,idx.low])
        
        if(abs(trade.qty)>0){
          order.new <- data.frame(
            date = cur.date,
            symbol = sym,
            tradeside = ifelse(trade.qty > 0,"buy","sell"),
            qty = trade.qty,
            price = trade.price,
            txn = trade.txn,
            position = -trade.qty * trade.price - trade.txn,
            holdratio = cur.ratio
          )  
        }
        
      }
      
      if(is.null(order.new)){
        
        cur.price <- as.numeric(mktdata[cur.date,idx.close])
        portf[[sym]][cur.date,"qty"] <- last.qty 
        portf[[sym]][cur.date,"avgPrice"] <- last.avgPrice
        portf[[sym]][cur.date,"curPrice"] <- cur.price
        portf[[sym]][cur.date,"realizedPos"] <- last.realizedPos
        portf[[sym]][cur.date,"unrealizedPos"] <- portf[[sym]][cur.date,"qty"] * cur.price - portf[[sym]][cur.date,"qty"]*portf[[sym]][cur.date,"avgPrice"]
        portf[[sym]][cur.date,"txn"]<-last.txn 
        portf[[sym]][cur.date,"position"] <- 0
        portf[[sym]][cur.date,"equity"] <- portf[[sym]][cur.date,"qty"] * cur.price
        portf[[sym]][cur.date,"holdratio"] <- last.ratio
        
      }else{
        if(abs(order.new$qty)>0){
          print(paste0(order.new$date ,"  ",order.new$tradeside,"  ",order.new$symbol,"  ",order.new$qty," @ ",order.new$price)) 
          orderbook <- rbind(orderbook,order.new)
          cur.price <- as.numeric(mktdata[cur.date,idx.close])
          portf[[sym]][cur.date,"qty"] <- last.qty + order.new$qty
          portf[[sym]][cur.date,"avgPrice"] <- ifelse(order.new$qty>0,(last.qty*last.avgPrice+order.new$qty*order.new$price)/(last.qty+order.new$qty),last.avgPrice)
          portf[[sym]][cur.date,"curPrice"] <- cur.price
          portf[[sym]][cur.date,"realizedPos"] <- last.realizedPos + ifelse(order.new$qty<0,order.new$position+order.new$txn+order.new$qty*last.avgPrice,0)
          portf[[sym]][cur.date,"unrealizedPos"] <- portf[[sym]][cur.date,"qty"] * cur.price - portf[[sym]][cur.date,"qty"]*portf[[sym]][cur.date,"avgPrice"]
          portf[[sym]][cur.date,"txn"]<-last.txn + order.new$txn
          portf[[sym]][cur.date,"position"] <-  order.new$position
          portf[[sym]][cur.date,"equity"] <- portf[[sym]][cur.date,"qty"] * cur.price
          portf[[sym]][cur.date,"holdratio"] <- cur.ratio
          
          
        }
      }
      
      
      
      cur.portfMat<-t(sapply(acct.syms,function(x)  portf[[x]][cur.date,c("realizedPos","unrealizedPos","txn","position","equity")]))
      cur.portfSum<-colSums(cur.portfMat)
      
      cur.portfNum <- sum(t(sapply(acct.syms,function(x)  portf[[x]][cur.date,c("qty")]>0)))
      
      
      
      portf[["summary"]][cur.date,c("realizedPos","unrealizedPos","txn","equity")]<-cur.portfSum[c("realizedPos","unrealizedPos","txn","equity")]
      portf[["summary"]][cur.date,c("position")]<-portf[["summary"]][last.date,c("position")] + cur.portfSum[c("position")]
      portf[["summary"]][cur.date,"total"] <- portf[["summary"]][cur.date,c("position")] + portf[["summary"]][cur.date,c("equity")] 
      
      portf[["summary"]][cur.date,"symNum"] <- as.numeric(cur.portfNum)
      
      portf[["summary"]][cur.date,"holdratio"] <- ifelse(sum(t(sapply(acct.syms,function(x)  portf[[x]][cur.date,c("holdratio")])))==0,
                                                         0, 
                                                         1- portf[[cash.sym]][cur.date,"holdratio"])
      
      
      
    }
  }
  
  assign("portf",portf,envir = .GlobalEnv)
  assign("orderbook",orderbook,envir=.GlobalEnv)
  
}
## -------------ADD New Trades----------------
