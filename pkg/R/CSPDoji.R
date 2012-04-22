CSPDoji <- function(TS, maxbodyCL=.1, maxshadowCL=.1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  BL <- abs(Cl(TS)-Op(TS))
  CL <- Hi(TS)-Lo(TS)
  BodyHi <- as.xts(apply(cbind(Op(TS),Cl(TS)),1,max))
  BodyLo <- as.xts(apply(cbind(Op(TS),Cl(TS)),1,min))
  Doji <- reclass(BL < CL* maxbodyCL, TS)
  DFDoji <- reclass(Doji & (Hi(TS)-BodyHi <= CL* maxshadowCL), TS)
  GSDoji <- reclass(Doji & (BodyLo-Lo(TS) <= CL* maxshadowCL), TS)
  result <- cbind(Doji, DFDoji, GSDoji)
  colnames(result) <- c("Doji", "DragonflyDoji", "GravestoneDoji")
  xtsAttributes(result) <- list(bars=1)
  return (result)
}
