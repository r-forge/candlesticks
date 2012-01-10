CSPPiercingPattern <- function(TS) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  LAGTS <- LagOC(TS, k=1)
  PiercingPattern <- eval (
    Op(LAGTS)>Cl(LAGTS) & Cl(TS)>Op(TS)
    & Cl(LAGTS)>Op(TS) & Cl(TS)>(Op(LAGTS)+Cl(LAGTS))/2
    & Op(LAGTS)>Cl(TS) )
  colnames(PiercingPattern) <- c("PiercingPattern")
  return(PiercingPattern)
}