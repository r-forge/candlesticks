CSPDoji <- function(TS, DojiBLRatio=.1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  Doji <- reclass(eval (abs(Op(TS)-Cl(TS))/(Hi(TS)-Lo(TS)) <= DojiBLRatio), TS)
  DFDoji <- reclass(eval (Doji & (Op(TS)==Hi(TS) | Cl(TS)==Hi(TS))), TS)
  GSDoji <- reclass(eval (Doji & (Op(TS)==Lo(TS) | Cl(TS)==Lo(TS))), TS)
  result <- cbind(Doji, DFDoji, GSDoji)
  colnames(result) <- c("Doji", "DragonflyDoji", "GravestoneDoji")
  xtsAttributes(result) <- list(bars=1)
  return (result)
}
