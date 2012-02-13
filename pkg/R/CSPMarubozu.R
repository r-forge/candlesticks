CSPMarubozu <- function(TS, n=20, threshold=1.5) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LCB <- CSPLongCandleBody(TS, n=n, threshold=threshold)
  WhiteMarubozu <- reclass(eval( LCB[,"LongWhiteCandleBody"] & Op(TS)==Lo(TS) & Cl(TS)==Hi(TS) ), TS)
  WhiteOpeningMarubozu <- reclass(eval( LCB[,"LongWhiteCandleBody"] & Op(TS)==Lo(TS) & Cl(TS)<Hi(TS) ), TS)
  WhiteClosingMarubozu <- reclass(eval( LCB[,"LongWhiteCandleBody"] & Op(TS)>Lo(TS) & Cl(TS)==Hi(TS) ), TS)
  BlackMarubozu <- reclass(eval( LCB[,"LongBlackCandleBody"] & Op(TS)==Hi(TS) & Cl(TS)==Lo(TS) ), TS)
  BlackOpeningMarubozu <- reclass(eval( LCB[,"LongBlackCandleBody"] & Op(TS)==Hi(TS) & Cl(TS)>Lo(TS) ), TS)
  BlackClosingMarubozu <- reclass(eval( LCB[,"LongBlackCandleBody"] & Op(TS)<Hi(TS) & Cl(TS)==Lo(TS) ), TS)
  result <- cbind(WhiteMarubozu, WhiteOpeningMarubozu, WhiteClosingMarubozu, 
                  BlackMarubozu, BlackOpeningMarubozu, BlackClosingMarubozu)
  colnames(result) <- c("WhiteMarubozu", "WhiteOpeningMarubozu", "WhiteClosingMarubozu", 
                        "BlackMarubozu", "BlackOpeningMarubozu", "BlackClosingMarubozu")
  xtsAttributes(result) <- list(bars=1)
  return(result)
}