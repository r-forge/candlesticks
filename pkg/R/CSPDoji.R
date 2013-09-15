CSPDoji <- function(TS, maxbodyCL=.1, maxshadowCL=.1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  BL <- abs(Cl(TS)-Op(TS))
  CL <- Hi(TS)-Lo(TS)
  BodyHi <- pmax(Op(TS), Cl(TS))
  BodyLo <- pmin(Op(TS), Cl(TS))
  Doji <- reclass(BL < CL* maxbodyCL, TS)
  DFDoji <- reclass(Doji & (Hi(TS)-BodyHi <= CL* maxshadowCL), TS)
  GSDoji <- reclass(Doji & (BodyLo-Lo(TS) <= CL* maxshadowCL), TS)
  result <- cbind(Doji, DFDoji, GSDoji)
  colnames(result) <- c("Doji", "DragonflyDoji", "GravestoneDoji")
  xtsAttributes(result) <- list(bars=1)
  return (result)
}
