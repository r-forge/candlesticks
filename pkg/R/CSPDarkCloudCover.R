CSPDarkCloudCover <- function(TS) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  LAGTS <- LagOC(TS, k=1)
  DarkCloudCover <- eval (
    Cl(LAGTS)>Op(LAGTS) & Op(TS)>Cl(TS)
    & Op(TS)>Cl(LAGTS) & (Op(LAGTS)+Cl(LAGTS))/2 > Cl(TS)
    & Cl(TS)>Op(LAGTS) )
  colnames(DarkCloudCover) <- c("DarkCloudCover")
  return(DarkCloudCover)
}