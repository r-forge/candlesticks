CSPThreeInside <- function(TS) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LAGTS2 <- Lag(Op(TS), k=2)   # first candle of formation
  LAGTS1 <- LagOHLC(TS, k=1)   # second candle
  LAGHARAMI <- CSPHarami(LAGTS1)
  TIUP <- reclass(
    LAGHARAMI[,1] &            # bullish harami
    Cl(TS)>Op(TS) &            # 3rd candle is white
    Cl(TS)>Cl(LAGTS1) &        # close of 3rd candle greater than close of 2nd candle
    Cl(TS)>LAGTS2              # close of 3rd candle greater than open of 1st candle
    , TS)
  TIDOWN <- reclass(
    LAGHARAMI[,2] &       # bearish harami
    Cl(TS)<Op(TS) &            # 3rd candle is black
    Cl(TS)<Cl(LAGTS1) &        # close of 3rd candle lower than close of 2nd candle
    Cl(TS)<LAGTS2              # close of 3rd candle lower than open of 1st candle
    , TS)
  result <- cbind(TIUP, TIDOWN)
  colnames(result) <- c("ThreeInsideUp", "ThreeInsideDown")
  xtsAttributes(result) <- list(bars=3)
  return(result)
}