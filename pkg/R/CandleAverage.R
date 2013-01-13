OHLC_Average <- function(TS) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  Av <- as.xts(apply(cbind(Op(TS), Hi(TS), Lo(TS), Cl(TS)), 1, mean))
  colnames(Av) <- c("OHLC_Average")
  return (reclass(Av, TS))
}

HLC_Average <- function(TS) {
  if (!is.HLC(TS)) {
    stop("Price series must contain High, Low and Close.")
  }
  Av <- as.xts(apply(cbind(Hi(TS), Lo(TS), Cl(TS)), 1, mean))
  colnames(Av) <- c("HLC_Average")
  return (reclass(Av, TS))
}

HL_Average <- function(TS) {
  if (!is.HL(TS)) {
    stop("Price series must contain High and Low.")
  }  
  Av <- as.xts(apply(cbind(Hi(TS), Lo(TS)), 1, mean))
  colnames(Av) <- c("HL_Average")
  return (reclass(Av, TS))
}