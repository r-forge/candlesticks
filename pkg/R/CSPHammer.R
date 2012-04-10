CSPHammer <- function(TS, minlowershadowCL=2/3, maxuppershadowCL=.1, minbodyCL=.1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  CL <- Hi(TS)-Lo(TS)
  BodyHi <- as.xts(apply(cbind(Op(TS),Cl(TS)),1,max))
  BodyLo <- as.xts(apply(cbind(Op(TS),Cl(TS)),1,min))
  Hammer <- reclass( eval (
    BodyLo-Lo(TS) > CL*minlowershadowCL &   # lower shadow greater than lowershadeCL*CandleLength
    Hi(TS)- BodyHi <= CL*maxuppershadowCL & # upper shadow missing or very short
    abs (Cl(TS)-Op(TS)) > CL*minbodyCL)     # Body length greater than minbodyCL*CandleLength
    ,TS)
  colnames(Hammer) <- c("Hammer")
  xtsAttributes(Hammer) <- list(bars=1)
  return (Hammer)
}
