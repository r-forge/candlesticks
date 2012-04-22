CSPInsideDay <- function(TS) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LAGTS <- LagOHLC(TS, k=1)
  result <- reclass( Hi(TS)<=Hi(LAGTS) & Lo(TS)>=Lo(LAGTS) , TS)
  colnames(result) <- "InsideDay"
  xtsAttributes(result) <- list(bars=2)
  return (result)
}

CSPOutsideDay <- function(TS) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LAGTS <- LagOHLC(TS, k=1)
  result <- reclass( Hi(TS)>Hi(LAGTS) & Lo(TS)<Lo(LAGTS) , TS)
  colnames(result) <- "OutsideDay"
  xtsAttributes(result) <- list(bars=2)
  return (result)
}
