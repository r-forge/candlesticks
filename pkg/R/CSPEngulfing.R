CSPEngulfing <- function(TS) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  LAGTS <- LagOC(TS, k=1)
  BullEngulfing <- eval( Op(LAGTS)>Cl(LAGTS) & Cl(TS)>Op(TS) & Cl(LAGTS)>=Op(TS) & Cl(TS)>=Op(LAGTS) )
  BearEngulfing <- eval( Cl(LAGTS)>Op(LAGTS) & Op(TS)>Cl(TS) & Op(LAGTS)>=Cl(TS) & Op(TS)>=Cl(LAGTS) )
  result <- cbind(BullEngulfing, BearEngulfing)
  colnames(result) <- c("Bull.Engulfing", "Bear.Engulfing")
  return(result)
}
