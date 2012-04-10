CSPMarubozu <- function(TS, n=20, ATRFactor=1, maxuppershadowCL=.1, maxlowershadowCL=.1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LongCandle = eval(CandleBodyLength(TS)[,"absCandleBodyLength"] > 
    ATR(cbind(Hi(TS), Lo(TS), Cl(TS)), n=n, maType="SMA")[,"atr"]*ATRFactor)
  CL <- Hi(TS)-Lo(TS)
  BodyHi <- as.xts(apply(cbind(Op(TS),Cl(TS)),1,max))
  BodyLo <- as.xts(apply(cbind(Op(TS),Cl(TS)),1,min))
  ShortShadow = eval(Hi(TS)-BodyHi <= CL*maxuppershadowCL & BodyLo-Lo(TS) <= CL*maxlowershadowCL)
  WhiteMarubozu <- reclass(eval( LongCandle & ShortShadow & Op(TS) < Cl(TS)), TS)
  BlackMarubozu <- reclass(eval( LongCandle & ShortShadow & Op(TS) > Cl(TS)), TS)
  result <- cbind(WhiteMarubozu, BlackMarubozu)
  colnames(result) <- c("WhiteMarubozu", "BlackMarubozu")
  xtsAttributes(result) <- list(bars=1)
  return(result)
}