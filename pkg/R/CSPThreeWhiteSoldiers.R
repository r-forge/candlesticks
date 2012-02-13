CSPThreeWhiteSoldiers <- function (TS, strict=TRUE, n=20, threshold=1.5) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  THREELWCB <- CSPNLongWhiteCandleBodies(TS, N=3, n=n, threshold=threshold)
  LAGTS <- LagOC(TS,k=0:2)
  result <- reclass(eval(THREELWCB[,1] & 
    Op(LAGTS)[,1] > Op(LAGTS)[,2] &
    Op(LAGTS)[,2] > Op(LAGTS)[,3] &
    Cl(LAGTS)[,1] > Cl(LAGTS)[,2] &
    Cl(LAGTS)[,2] > Cl(LAGTS)[,3]), TS)
  # in strict mode the candles should open within the previous
  # candle's body
  if (strict==TRUE) {
    result <- reclass(eval(result &
      Op(LAGTS)[,1] <= Cl(LAGTS)[,2] &
      Op(LAGTS)[,2] <= Cl(LAGTS)[,3]), TS)
  }
  colnames(result) <- c("ThreeWhiteSoldiers")
  xtsAttributes(result) <- list(bars=3)
  return (result)
}

CSPThreeBlackCrows <- function (TS, strict=TRUE, n=20, threshold=1.5) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  THREELBCB <- CSPNLongBlackCandleBodies(TS, N=3, n=n, threshold=threshold)
  LAGTS <- LagOC(TS,k=0:2)
  result <- reclass(eval(THREELBCB[,1] & 
    Op(LAGTS)[,1] < Op(LAGTS)[,2] &
    Op(LAGTS)[,2] < Op(LAGTS)[,3] &
    Cl(LAGTS)[,1] < Cl(LAGTS)[,2] &
    Cl(LAGTS)[,2] < Cl(LAGTS)[,3]), TS)
  # in strict mode the candles should open within the previous
  # candle's body
  if (strict==TRUE) {
    result <- reclass(eval(result &
      Op(LAGTS)[,1] >= Cl(LAGTS)[,2] &
      Op(LAGTS)[,2] >= Cl(LAGTS)[,3]), TS)
  }
  colnames(result) <- c("ThreeBlackCrows")
  xtsAttributes(result) <- list(bars=3)
  return (result)
}