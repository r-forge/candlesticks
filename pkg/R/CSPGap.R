CSPGap <- function (TS, ignoreShadows=FALSE) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LAGTS <- LagOHLC(TS, k=1)
  if (ignoreShadows==TRUE) {
    TSOC <- cbind(Op(TS), Cl(TS))
    LAGTSOC <- cbind(Op(LAGTS), Cl(LAGTS))
    UPGAP <- eval(as.xts(apply(LAGTSOC,1,max)) < as.xts(apply(TSOC,1,min)))
    DOWNGAP <- eval(as.xts(apply(LAGTSOC,1,min)) > as.xts(apply(TSOC,1,max)))    
  }
  else {
    UPGAP <- eval(Lo(TS) > Hi(LAGTS))
    DOWNGAP <- eval(Hi(TS) < Lo(LAGTS))
  }
  result <- cbind(UPGAP,DOWNGAP)
  colnames(result) <- c("GapUp", "GapDown")
  return (result)
}