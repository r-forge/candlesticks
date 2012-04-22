CSPThreeWhiteSoldiers <- function (TS, strict=TRUE, n=20,  minbodysizeMedian=1) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  THREELWCB <- CSPNLongWhiteCandleBodies(TS, N=3, n=n, threshold=minbodysizeMedian)
  LAGTS <- LagOC(TS,k=0:2)  # lags 0, 1, 2 periods
  result <- reclass( THREELWCB[,1] & 
    Op(LAGTS)[,1] > Op(LAGTS)[,2] & # third open higher than second
    Op(LAGTS)[,2] > Op(LAGTS)[,3] & # second open higher than first
    Cl(LAGTS)[,1] > Cl(LAGTS)[,2] & # third close higher than second
    Cl(LAGTS)[,2] > Cl(LAGTS)[,3] , TS) # second close higher than first
  # in strict mode the candles should open within the previous
  # candle's body
  if (strict==TRUE) {
    result <- reclass( result &
      Op(LAGTS)[,1] <= Cl(LAGTS)[,2] & # third open within second candle body
      Op(LAGTS)[,2] <= Cl(LAGTS)[,3] , TS) # second open within first candle body
  }
  colnames(result) <- c("ThreeWhiteSoldiers")
  xtsAttributes(result) <- list(bars=3)
  return (result)
}

CSPThreeBlackCrows <- function (TS, strict=TRUE, n=20,  minbodysizeMedian=1) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  THREELBCB <- CSPNLongBlackCandleBodies(TS, N=3, n=n, threshold=minbodysizeMedian)
  LAGTS <- LagOC(TS,k=0:2)
  result <- reclass( THREELBCB[,1] & 
    Op(LAGTS)[,1] < Op(LAGTS)[,2] &
    Op(LAGTS)[,2] < Op(LAGTS)[,3] &
    Cl(LAGTS)[,1] < Cl(LAGTS)[,2] &
    Cl(LAGTS)[,2] < Cl(LAGTS)[,3] , TS)
  # in strict mode the candles should open within the previous
  # candle's body
  if (strict==TRUE) {
    result <- reclass( result &
      Op(LAGTS)[,1] >= Cl(LAGTS)[,2] &
      Op(LAGTS)[,2] >= Cl(LAGTS)[,3] , TS)
  }
  colnames(result) <- c("ThreeBlackCrows")
  xtsAttributes(result) <- list(bars=3)
  return (result)
}