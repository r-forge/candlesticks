CSPStar <- function (TS, n=20, minbodysizeMedian=1, maxbodysizeMedian=1) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  LAG2TS <- LagOC(TS, k=2)
  LAG1TS <- LagOC(TS, k=1)  
  LCB2 <- CSPLongCandleBody (LAG2TS, n=n, threshold=minbodysizeMedian) # first candle body is longer than average
  GAP1 <- CSPGap(LAG1TS, ignoreShadows=TRUE) # gap between the first and second candle
  SCB1 <- CSPShortCandleBody (LAG1TS, n=n, threshold=maxbodysizeMedian)# second candle body is shorter than average
  MorningStar <- reclass(
    LCB2[,2] &                     # 1st candle: long black candle body
    GAP1[,2] &                     # gap down from 1st to 2nd candle
    (SCB1[,1] | SCB1[,2]) &        # 2nd candle: short black or white candle body
    Cl(TS) > Op(TS) &              # 3rd candle is white
    Cl(TS) > (Op(LAG2TS)+Cl(LAG2TS))/2 # 3rd candle closes above middle of 1st candle body
    , TS)
  EveningStar <- reclass(
    LCB2[,1] &                     # 1st candle: long white candle body
    GAP1[,1] &                     # gap up from 1st to 2nd candle
    (SCB1[,1] | SCB1[,2]) &        # 2nd candle: short black or white candle body
    Cl(TS) < Op(TS) &              # third candle is white
    Cl(TS) < (Op(LAG2TS)+Cl(LAG2TS))/2 # 3rd candle closes below middle of 1st candle body
    , TS)
  result <- cbind(MorningStar, EveningStar)
  colnames(result) <- c("MorningStar", "EveningStar")
  xtsAttributes(result) <- list(bars=3)
  return (result)
}