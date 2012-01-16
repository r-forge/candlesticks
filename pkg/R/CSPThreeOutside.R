CSPThreeOutside <- function(TS) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  LAGTS1 <- LagOHLC(TS, k=1)      # second candle
  LAGENGULFING <- CSPEngulfing(LAGTS1)
  TOUP <- eval(LAGENGULFING[,1] & # bullish engulfing
    Cl(TS)>Op(TS) &               # 3rd candle is white
    Cl(TS)>Cl(LAGTS1))            # 3rd candle closes above 2nd candle
  TODOWN <- eval(LAGENGULFING[,2] & # bearish engulfing
    Cl(TS)<Op(TS) &               # 3rd candle is black
    Cl(TS)<Cl(LAGTS1))            # 3rd candle closes below 2nd candle
  result <- cbind(TOUP, TODOWN)
  colnames(result) <- c("ThreeOutsideUp", "ThreeOutsideDown")
  return(result)
}