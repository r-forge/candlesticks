CSPHarami <- function(TS, n=20, minbodysizeMedian=1) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LAGTS <- LagOC(TS, k=1)
  LongCandleBody <- CSPLongCandleBody(LAGTS, n=n, threshold=minbodysizeMedian)
  BullHarami <- reclass(
    LongCandleBody[,2] &                     # body of mother candle is black and longer than average
    Cl(TS)>Op(TS) &                          # second candle is white
    Op(LAGTS)>Cl(TS) & Cl(LAGTS)<Op(TS)      # second body is within first body
    , TS)
  BearHarami <- reclass(
    LongCandleBody[,1] &                     # body of mother candle is white and longer than average
    Cl(TS)<Op(TS) &                          # second candle is white
    Op(LAGTS)<Cl(TS) & Cl(LAGTS)>Op(TS)      # second body is within first body
    , TS)
  result <- cbind(BullHarami, BearHarami)
  colnames(result) <- c("Bull.Harami", "Bear.Harami")
  xtsAttributes(result) <- list(bars=2)
  return(result)
}
