CSPMarubozu <- function(TS, n=20, threshold=1.5) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LCB <- CSPLongCandleBody(TS, n=n, threshold=threshold)
  WhiteMarubozu <- eval( LCB[,"LongWhiteCandleBody"] & Op(TS)==Lo(TS) & Cl(TS)==Hi(TS) )
  WhiteOpeningMarubozu <- eval( LCB[,"LongWhiteCandleBody"] & Op(TS)==Lo(TS) & Cl(TS)<Hi(TS) )
  WhiteClosingMarubozu <- eval( LCB[,"LongWhiteCandleBody"] & Op(TS)>Lo(TS) & Cl(TS)==Hi(TS) ) 
  BlackMarubozu <- eval( LCB[,"LongBlackCandleBody"] & Op(TS)==Hi(TS) & Cl(TS)==Lo(TS) )
  BlackOpeningMarubozu <- eval( LCB[,"LongBlackCandleBody"] & Op(TS)==Hi(TS) & Cl(TS)>Lo(TS) ) 
  BlackClosingMarubozu <- eval( LCB[,"LongBlackCandleBody"] & Op(TS)<Hi(TS) & Cl(TS)==Lo(TS) )
  result <- cbind(WhiteMarubozu, WhiteOpeningMarubozu, WhiteClosingMarubozu, 
                  BlackMarubozu, BlackOpeningMarubozu, BlackClosingMarubozu)
  colnames(result) <- c("WhiteMarubozu", "WhiteOpeningMarubozu", "WhiteClosingMarubozu", 
                        "BlackMarubozu", "BlackOpeningMarubozu", "BlackClosingMarubozu")
  return(result)
}