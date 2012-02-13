CSPLongCandle <- function(TS, n=20, threshold=1.5) {
  if (!is.OHLC (TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  CL <- CandleLength (TS)
  CLMedian <- runMedian (CL[,1], n=n) # use relative CandleLength
  LongWhiteCandle <- reclass(eval (CL[,1] >= CLMedian*threshold & Cl(TS) >= Op(TS)), TS)
  LongBlackCandle <- reclass(eval (CL[,1] >= CLMedian*threshold & Op(TS) > Cl(TS)), TS)
  result <- cbind (LongWhiteCandle, LongBlackCandle)
  colnames (result) <- c("LongWhiteCandle", "LongBlackCandle")
  xtsAttributes(result) <- list(bars=1)
  return (result)
}

CSPLongCandleBody <- function(TS, n=20, threshold=1.5) {
  if (!is.OC (TS)) {
    stop("Price series must contain Open and Close.")
  }
  CBL <- CandleBodyLength (TS)
  CBLMedian <- runMedian (CBL[,1], n=n) # use relative CandleBodyLength
  LongWhiteCandleBody <- reclass(eval (CBL[,1] >= CBLMedian*threshold & Cl(TS) >= Op(TS)), TS)
  LongBlackCandleBody <- reclass(eval (CBL[,1] >= CBLMedian*threshold & Op(TS) > Cl(TS)), TS)
  result <- cbind (LongWhiteCandleBody, LongBlackCandleBody)
  colnames (result) <- c("LongWhiteCandleBody", "LongBlackCandleBody")
  xtsAttributes(result) <- list(bars=1)
  return (result)
}

CSPShortCandle <- function(TS, n=20, threshold=1) {
  if (!is.OHLC (TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  CL <- CandleLength (TS)
  CLMedian <- runMedian (CL[,1], n=n) # use relative CandleLength
  ShortWhiteCandle <- reclass(eval (CL[,1] < CLMedian*threshold & Cl(TS) >= Op(TS)), TS)
  ShortBlackCandle <- reclass(eval (CL[,1] < CLMedian*threshold & Op(TS) > Cl(TS)), TS)
  result <- cbind (ShortWhiteCandle, ShortBlackCandle)
  colnames (result) <- c("LongWhiteCandle", "LongBlackCandle")
  xtsAttributes(result) <- list(bars=1)
  return (result)
}

CSPShortCandleBody <- function(TS, n=20, threshold=1) {
  if (!is.OC (TS)) {
    stop("Price series must contain Open and Close.")
  }
  CBL <- CandleBodyLength (TS)
  CBLMedian <- runMedian (CBL[,1], n=n) # use relative CandleBodyLength
  ShortWhiteCandleBody <- reclass(eval (CBL[,1] < CBLMedian*threshold & Cl(TS) >= Op(TS)), TS)
  ShortBlackCandleBody <- reclass(eval (CBL[,1] < CBLMedian*threshold & Op(TS) > Cl(TS)), TS)
  result <- cbind (ShortWhiteCandleBody, ShortBlackCandleBody)
  colnames (result) <- c("ShortWhiteCandleBody", "ShortBlackCandleBody")
  xtsAttributes(result) <- list(bars=1)
  return (result)
}
