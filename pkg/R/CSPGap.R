CSPGap <- function (TS, ignoreShadows=FALSE) {
  if (ignoreShadows==TRUE) {
    if (!is.OC(TS)) {
      stop("Price series must contain Open and Close.")
    }
    LAGTS <- LagOC(TS, k=1)
    TSOC <- cbind(Op(TS), Cl(TS))
    LAGTSOC <- cbind(Op(LAGTS), Cl(LAGTS))
    UPGAP <- eval(as.xts(apply(LAGTSOC,1,max)) < as.xts(apply(TSOC,1,min)))
    DOWNGAP <- eval(as.xts(apply(LAGTSOC,1,min)) > as.xts(apply(TSOC,1,max)))    
  }
  else {
    if (!is.OHLC(TS)) {
      stop("Price series must contain Open, High, Low and Close.")
    }
    LAGTS <- LagOHLC(TS, k=1)
    UPGAP <- eval(Lo(TS) > Hi(LAGTS))
    DOWNGAP <- eval(Hi(TS) < Lo(LAGTS))
  }
  result <- cbind(UPGAP,DOWNGAP)
  colnames(result) <- c("GapUp", "GapDown")
  return (result)
}