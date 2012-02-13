CSPTasukiGap <- function (TS) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LAG2TS <- LagOHLC(TS, k=2)
  LAG1TS <- LagOHLC(TS, k=1)
  GAP1 <- CSPGap(LAG1TS, ignoreShadows=FALSE)
  UTG <- reclass(
    eval(Op(LAG2TS) < Cl(LAG2TS) &        # 1st candle: white
    GAP1[,1] &                            # Up Gap btwn 1st and 2nd candle
    Op(LAG1TS) < Cl(LAG1TS) &             # 2nd candle: white
    Op(TS) < Cl(LAG1TS) & Op(TS) > Op(LAG1TS)  & # 3rd candle opens within 2nd candle's body
    Cl(TS) < Lo(LAG1TS) & Cl(TS) > Hi(LAG2TS)),  # 3rd candle closes within gap of 1st and 2nd candle
    TS)
  DTG <- reclass(
    eval(Op(LAG2TS) > Cl(LAG2TS) &        # 1st candle: black
    GAP1[,2] &                            # Down Gap btwn 1st and 2nd candle
    Op(LAG1TS) > Cl(LAG1TS) &             # 2nd candle: black
    Op(TS) > Cl(LAG1TS) & Op(TS) < Op(LAG1TS)  & # 3rd candle opens within 2nd candle's body
    Cl(TS) > Hi(LAG1TS) & Cl(TS) < Lo(LAG2TS)),  # 3rd candle closes within gap of 1st and 2nd candle
    TS)
  result <- cbind(UTG, DTG)
  colnames(result) <- c("UpsideTasukiGap", "DownsideTasukiGap")
  xtsAttributes(result) <- list(bars=3)
  return (result)
}