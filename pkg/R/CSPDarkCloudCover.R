CSPDarkCloudCover <- function(TS, n=20, minbodysizeMedian=1) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  LAGTS <- LagOC(TS, k=1)
  LongCandleBody <- CSPLongCandleBody(LAGTS, n=n, threshold=minbodysizeMedian)
  DarkCloudCover <- reclass(eval (
    LongCandleBody[,"LongWhiteCandleBody"] & # first candle is white and longer than median of past n candles
    Op(TS)>Cl(LAGTS) &                 # second candle opens higher than close of 1st candle
    (Op(LAGTS)+Cl(LAGTS))/2 >= Cl(TS) &# second candle closes at or below half of 1st candles' body 
    Cl(TS)>Op(LAGTS) ), TS)            # close of second candle is higher than open of 1st candle
  colnames(DarkCloudCover) <- c("DarkCloudCover")
  xtsAttributes(DarkCloudCover) <- list(bars=2)
  return(DarkCloudCover)
}