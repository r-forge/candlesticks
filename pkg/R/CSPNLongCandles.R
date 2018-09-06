CSPNLongWhiteCandles <- function(TS, N=2, n=20, threshold=1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  if (N<1) {
    stop("N has to be a integer >= 1")
  }
  LWC <- CSPLongCandle(TS, n=n, threshold=threshold)[,1] # LongWhiteCandle
  result <- reclass(as.xts(apply(stats::lag(LWC,k=0:(N-1)),1,all)), TS)
  colnames(result) <- paste(N, "LongWhiteCandles", sep="")
  xtsAttributes(result) <- list(bars=N)
  return (result)
}

CSPNLongBlackCandles <- function(TS, N=2, n=20, threshold=1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  if (N<1) {
    stop("N has to be a integer >= 1")
  }
  LBC <- CSPLongCandle(TS, n=n, threshold=threshold)[,2] # LongBlackCandle
  result <- reclass(as.xts(apply(stats::lag(LBC,k=0:(N-1)),1,all)), TS)
  colnames(result) <- paste(N, "LongBlackCandles", sep="")
  xtsAttributes(result) <- list(bars=N)
  return (result)
}

CSPNLongWhiteCandleBodies <- function(TS, N=2, n=20, threshold=1) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  if (N<1) {
    stop("N has to be a integer >= 1")
  }
  LWCB <- CSPLongCandleBody(TS, n=n, threshold=threshold)[,1] # LongWhiteCandleBody
  result <- reclass(as.xts(apply(stats::lag(LWCB,k=0:(N-1)),1,all)), TS)
  colnames(result) <- paste(N, "LongWhiteCandleBodies", sep="")
  xtsAttributes(result) <- list(bars=N)
  return (result)
}

CSPNLongBlackCandleBodies <- function(TS, N=2, n=20, threshold=1) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  if (N<1) {
    stop("N has to be a integer >= 1")
  }
  LBCB <- CSPLongCandleBody(TS, n=n, threshold=threshold)[,2] # LongBlackCandleBody
  result <- reclass(as.xts(apply(stats::lag(LBCB,k=0:(N-1)),1,all)), TS)
  colnames(result) <- paste(N, "LongBlackCandleBodies", sep="")
  xtsAttributes(result) <- list(bars=N)
  return (result)
}
