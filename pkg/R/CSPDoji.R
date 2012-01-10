CSPDoji <- function(TS, DojiBLRatio=.1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  Doji <- eval ( abs(Op(TS)-Cl(TS))/(Hi(TS)-Lo(TS)) <= DojiBLRatio)
  colnames(Doji) <- c("Doji")
  return (Doji)
}
