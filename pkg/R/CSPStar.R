CSPStar <- function (TS, n=20, lthreshold=1.5, sthreshold=1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LAG2TS <- LagOHLC(TS, k=2)
  LAG1TS <- LagOHLC(TS, k=1)
  LCB2 <- CSPLongCandleBody (LAG2TS, n=n, threshold=lthreshold)
  GAP1 <- CSPGap(LAG1TS, ignoreShadows=TRUE)
  SCB1 <- CSPShortCandleBody (LAG1TS, n=n, threshold=sthreshold)
  MorningStar <- eval(LCB2[,2] &   # 1st candle: long black candle body
    GAP1[,2] &                     # gap down from 1st to 2nd candle
    (SCB1[,1] | SCB1[,2]) &        # 2nd candle: short black or white candle body
    Cl(TS) > (Op(LAG2TS)+Cl(LAG2TS))/2) # 3rd candle closes above middle of 1st candle body
  EveningStar <- eval(LCB2[,1] &   # 1st candle: long white candle body
    GAP1[,1] &                     # gap up from 1st to 2nd candle
    (SCB1[,1] | SCB1[,2]) &        # 2nd candle: short black or white candle body
    Cl(TS) < (Op(LAG2TS)+Cl(LAG2TS))/2) # 3rd candle closes below middle of 1st candle body
  result <- cbind(MorningStar, EveningStar)
  colnames(result) <- c("MorningStar", "EveningStar")
  return (result)
}