CSPThreeMethods <- function (TS, n=20, threshold=1.5) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LAG4TS <- LagOHLC(TS, k=4)
  LAG3TS <- LagOC(TS, k=3)
  LAG1TS <- LagOC(TS, k=1)
  MAXOP <- stats::lag(runMax(Op(TS), n=3), k=1) # max open for middle 3 candles
  MAXCL <- stats::lag(runMax(Cl(TS), n=3), k=1) # max close for middle 3 candles
  MINOP <- stats::lag(runMin(Op(TS), n=3), k=1) # min open for middle 3 candles
  MINCL <- stats::lag(runMin(Cl(TS), n=3), k=1) # min close for middle 3 candles
  LC4 <- CSPLongCandleBody(LAG4TS, n=n, threshold=threshold)
  LC0 <- CSPLongCandleBody(TS, n=n, threshold=threshold)
  RTM <- reclass(
    LC4[,1] &                     # 1st candle: long white candle body
    Op(LAG3TS) > Cl(LAG3TS) &     # 2nd candle: black candle
    Op(LAG1TS) > Cl(LAG1TS) &     # 4th candle: black candle
    MAXOP < Hi(LAG4TS) & MAXOP > Lo(LAG4TS) & # candle bodies 2,3,4 within range of 1st candle
    MAXCL < Hi(LAG4TS) & MAXCL > Lo(LAG4TS) &
    MINOP < Hi(LAG4TS) & MINOP > Lo(LAG4TS) &
    MINCL < Hi(LAG4TS) & MINCL > Lo(LAG4TS) &
    LC0[,1] & Cl(TS) > Cl(LAG4TS) # 5th candle: long white candle body that closes higher than 1st candle
    , TS)
  FTM <- reclass(
    LC4[,2] &                      # 1st candle: long black candle body
    Op(LAG3TS) < Cl(LAG3TS) &      # 2nd candle: white candle
    Op(LAG1TS) < Cl(LAG1TS) &      # 4th candle: white candle
    MAXOP < Hi(LAG4TS) & MAXOP > Lo(LAG4TS) & # candle bodies 2,3,4 within range of 1st candle
    MAXCL < Hi(LAG4TS) & MAXCL > Lo(LAG4TS) &
    MINOP < Hi(LAG4TS) & MINOP > Lo(LAG4TS) &
    MINCL < Hi(LAG4TS) & MINCL > Lo(LAG4TS) &
    LC0[,2] & Cl(TS) < Cl(LAG4TS) # 5th candle: long black candle  body that closes lower than 1st candle
    , TS)
  result <- cbind (RTM, FTM)
  colnames (result) <- c("RisingThreeMethods", "FallingThreeMethods")
  xtsAttributes(result) <- list(bars=5)
  return (result)
}
