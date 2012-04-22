CSPPiercingPattern <- function(TS, n=20, minbodysizeMedian=1) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  LAGTS <- LagOC(TS, k=1)
  LongCandleBody <- CSPLongCandleBody(LAGTS, n=n, threshold=minbodysizeMedian)

  PiercingPattern <- reclass(
    LongCandleBody[,"LongBlackCandleBody"] & # first candle is black and longer than median of past n candles
    Op(TS)<Cl(LAGTS) &                 # second candle opens lower than close of 1st candle
    Cl(TS)>=(Op(LAGTS)+Cl(LAGTS))/2 &  # second candle closes at or higher than half of 1st candles' body
    Cl(TS)<Op(LAGTS) , TS)             # close of second candle is lower than open of 1st candle
  colnames(PiercingPattern) <- c("PiercingPattern")
  xtsAttributes(PiercingPattern) <- list(bars=2)
  return(PiercingPattern)
}