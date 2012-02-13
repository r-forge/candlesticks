CSPKicking <- function(TS, ignoreShadows=TRUE, n=20, threshold=1.5) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  TSGAP <- CSPGap(TS, ignoreShadows=ignoreShadows)
  if (ignoreShadows==FALSE) {
    MB <- CSPMarubozu(TS, n=n, threshold=threshold)
    WMB1 <- Lag(MB[,1], k=1)
    BMB1 <- Lag(MB[,4], k=1)
    BULLK <- reclass( 
      eval(TSGAP[,1] &         # Gap Up
      BMB1 & MB[,1]),          # 1st candle is black marubozu, 2nd candle is white marubozu
      TS)
    BEARK <- reclass(
      eval(TSGAP[,2] &         # Gap Down
      WMB1 & MB[,4]),          # 1st candle is white marubozu, 2nd candle is black marubozu
      TS)
  } else if (ignoreShadows==TRUE) {
    LCB <- CSPLongCandleBody(TS, n=n, threshold=threshold)
    LWCB1 <- Lag(LCB[,1], k=1)
    LBCB1 <- Lag(LCB[,2], k=1)
    BULLK <- reclass(
      eval(TSGAP[,1] &         # Gap Up
      LBCB1 & LCB[,1]),        # 1st candle has long black candle body, 2nd has long white candle body
      TS)
    BEARK <- reclass(
      eval(TSGAP[,2] &         # Gap Down
      LWCB1 & LCB[,2]),        # 1st candle has long white candle body, 2nd has long black candle body
      TS)
  } else {
    stop("ignoreShadows must be either TRUE or FALSE")
  }
  result <- cbind (BULLK, BEARK)
  colnames (result) <- (c("Bull.Kicking", "Bear.Kicking"))
  xtsAttributes(result) <- list(bars=2)
  return (result)
}