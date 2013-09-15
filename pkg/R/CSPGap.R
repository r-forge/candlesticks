CSPGap <- function (TS, ignoreShadows=FALSE) {
  if (ignoreShadows==TRUE) {
    if (!is.OC(TS)) {
      stop("Price series must contain Open and Close.")
    }
    LAGTS <- LagOC(TS, k=1)
    TSOC <- cbind(Op(TS), Cl(TS))
    LAGTSOC <- cbind(Op(LAGTS), Cl(LAGTS))
    UPGAP <- reclass( pmax(Op(LAGTSOC), Cl(LAGTSOC)) < pmin(Op(TSOC), Cl(TSOC)) , TS)
    DOWNGAP <- reclass( pmin(Op(LAGTSOC), Cl(LAGTSOC)) > pmax(Op(TSOC), Cl(TSOC)), TS)
  }
  else if (ignoreShadows==FALSE) {
    if (!is.OHLC(TS)) {
      stop("Price series must contain Open, High, Low and Close.")
    }
    LAGTS <- LagOHLC(TS, k=1)
    UPGAP <- reclass( Lo(TS) > Hi(LAGTS) , TS)
    DOWNGAP <- reclass( Hi(TS) < Lo(LAGTS) , TS)
  }
  result <- cbind(UPGAP, DOWNGAP)
  colnames(result) <- c("GapUp", "GapDown")
  xtsAttributes(result) <- list(bars=2)
  return (result)
}