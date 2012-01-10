CSPLongCandle <- function(TS, n=20, threshold=1.5) {
  if (!is.OHLC (TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  CL <- CandleLength (TS)
  CLMedian <- runMedian (CL[,1], n=n) # use relative CandleLength
  LongWhiteCandle <- eval (CL[,1] >= CLMedian*threshold & Cl(TS) >= Op(TS))
  LongBlackCandle <- eval (CL[,1] >= CLMedian*threshold & Op(TS) > Cl(TS))
  result <- cbind (LongWhiteCandle, LongBlackCandle)
  colnames (result) <- c("LongWhiteCandle", "LongBlackCandle")
  return (result)
}

CSPLongCandleBody <- function(TS, n=20, threshold=1.5) {
  if (!is.OC (TS)) {
    stop("Price series must contain Open and Close.")
  }
  CBL <- CandleBodyLength (TS)
  CBLMedian <- runMedian (CBL[,1], n=n) # use relative CandleBodyLength
  LongWhiteCandleBody <- eval (CBL[,1] >= CBLMedian*threshold & Cl(TS) >= Op(TS))
  LongBlackCandleBody <- eval (CBL[,1] >= CBLMedian*threshold & Op(TS) > Cl(TS))
  result <- cbind (LongWhiteCandleBody, LongBlackCandleBody)
  colnames (result) <- c("LongWhiteCandleBody", "LongBlackCandleBody")
  return (result)
}

CSPShortCandle <- function(TS, n=20, threshold=1) {
  if (!is.OHLC (TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  CL <- CandleLength (TS)
  CLMedian <- runMedian (CL[,1], n=n) # use relative CandleLength
  ShortWhiteCandle <- eval (CL[,1] < CLMedian*threshold & Cl(TS) >= Op(TS))
  ShortBlackCandle <- eval (CL[,1] < CLMedian*threshold & Op(TS) > Cl(TS))
  result <- cbind (ShortWhiteCandle, ShortBlackCandle)
  colnames (result) <- c("LongWhiteCandle", "LongBlackCandle")
  return (result)
}

CSPShortCandleBody <- function(TS, n=20, threshold=1) {
  if (!is.OC (TS)) {
    stop("Price series must contain Open and Close.")
  }
  CBL <- CandleBodyLength (TS)
  CBLMedian <- runMedian (CBL[,1], n=n) # use relative CandleBodyLength
  ShortWhiteCandleBody <- eval (CBL[,1] < CBLMedian*threshold & Cl(TS) >= Op(TS))
  ShortBlackCandleBody <- eval (CBL[,1] < CBLMedian*threshold & Op(TS) > Cl(TS))
  result <- cbind (ShortWhiteCandleBody, ShortBlackCandleBody)
  colnames (result) <- c("ShortWhiteCandleBody", "ShortBlackCandleBody")
  return (result)
}
