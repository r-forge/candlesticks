CSPNLongWhiteCandles <- function(TS, N=2, n=20, threshold=1.5) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  if (N<1) {
    stop("N has to be a integer >= 1")
  }
  LWC <- CSPLongCandle(TS, n=n, threshold=threshold)[,1] # LongWhiteCandle
  result <- as.xts(apply(Lag(LWC,k=0:(N-1)),1,all))
  colnames(result) <- paste(N, "LongWhiteCandles", sep="")
  return (result)
}

CSPNLongBlackCandles <- function(TS, N=2, n=20, threshold=1.5) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  if (N<1) {
    stop("N has to be a integer >= 1")
  }
  LBC <- CSPLongCandle(TS, n=n, threshold=threshold)[,2] # LongBlackCandle
  result <- as.xts(apply(Lag(LBC,k=0:(N-1)),1,all))
  colnames(result) <- paste(N, "LongBlackCandles", sep="")
  return (result)
}

CSPNLongWhiteCandleBodies <- function(TS, N=2, n=20, threshold=1.5) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  if (N<1) {
    stop("N has to be a integer >= 1")
  }
  LWCB <- CSPLongCandleBody(TS, n=n, threshold=threshold)[,1] # LongWhiteCandleBody
  result <- as.xts(apply(Lag(LWCB,k=0:(N-1)),1,all))
  colnames(result) <- paste(N, "LongWhiteCandleBodies", sep="")
  return (result)
}

CSPNLongBlackCandleBodies <- function(TS, N=2, n=20, threshold=1.5) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  if (N<1) {
    stop("N has to be a integer >= 1")
  }
  LBCB <- CSPLongCandleBody(TS, n=n, threshold=threshold)[,2] # LongBlackCandleBody
  result <- as.xts(apply(Lag(LBCB,k=0:(N-1)),1,all))
  colnames(result) <- paste(N, "LongBlackCandleBodies", sep="")
  return (result)
}
