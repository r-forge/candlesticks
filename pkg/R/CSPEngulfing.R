CSPEngulfing <- function(TS) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  LAGTS <- LagOC(TS, k=1)
  BullEngulfing <- reclass( Op(LAGTS)>Cl(LAGTS) & Cl(TS)>Op(TS) & Cl(LAGTS)>=Op(TS) & Cl(TS)>=Op(LAGTS) , TS)
  BearEngulfing <- reclass( Cl(LAGTS)>Op(LAGTS) & Op(TS)>Cl(TS) & Op(LAGTS)>=Cl(TS) & Op(TS)>=Cl(LAGTS) , TS)
  result <- cbind(BullEngulfing, BearEngulfing)
  colnames(result) <- c("Bull.Engulfing", "Bear.Engulfing")
  xtsAttributes(result) <- list(bars=2)
  return(result)
}
