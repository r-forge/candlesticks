CSPInvertedHammer <- function(TS, minuppershadowCL=2/3, maxlowershadowCL=.1, minbodyCL=.1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  CL <- Hi(TS)-Lo(TS)
  BodyHi <- as.xts(apply(cbind(Op(TS),Cl(TS)),1,max))
  BodyLo <- as.xts(apply(cbind(Op(TS),Cl(TS)),1,min))
  InvertedHammer <- reclass(
    Hi(TS)- BodyHi > CL*minuppershadowCL &   # upper shadow greater than lowershadowCL*CandleLength
    BodyLo- Lo(TS) <= CL*maxlowershadowCL &  # lower shadow missing or very short
    abs (Cl(TS)-Op(TS)) > CL*minbodyCL       # Body length greater than minbodyCL*CandleLength
    ,TS)
  colnames(InvertedHammer) <- c("InvertedHammer")
  xtsAttributes(InvertedHammer) <- list(bars=1)
  return (InvertedHammer)
}
