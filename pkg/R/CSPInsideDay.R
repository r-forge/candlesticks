CSPInsideDay <- function(TS) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LAGTS <- LagOHLC(TS, k=1)
  result <- eval(Hi(TS)<=Hi(LAGTS) & Lo(TS)>=Lo(LAGTS))
  colnames(result) <- "InsideDay"
  return (result)
}

CSPOutsideDay <- function(TS) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LAGTS <- LagOHLC(TS, k=1)
  result <- eval(Hi(TS)>Hi(LAGTS) & Lo(TS)<Lo(LAGTS))
  colnames(result) <- "OutsideDay"
  return (result)
}
