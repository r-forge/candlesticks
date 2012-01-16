CSPDoji <- function(TS, DojiBLRatio=.1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  Doji <- eval (abs(Op(TS)-Cl(TS))/(Hi(TS)-Lo(TS)) <= DojiBLRatio)
  DFDoji <- eval (Doji & (Op(TS)==Hi(TS) | Cl(TS)==Hi(TS)))
  GSDoji <- eval (Doji & (Op(TS)==Lo(TS) | Cl(TS)==Lo(TS)))  
  result <- cbind(Doji, DFDoji, GSDoji)
  colnames(result) <- c("Doji", "DragonflyDoji", "GravestoneDoji")
  return (result)
}
