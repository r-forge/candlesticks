CSPDarkCloudCover <- function(TS) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  LAGTS <- LagOC(TS, k=1)
  DarkCloudCover <- reclass(eval (
    Cl(LAGTS)>Op(LAGTS) & Op(TS)>Cl(TS)
    & Op(TS)>Cl(LAGTS) & (Op(LAGTS)+Cl(LAGTS))/2 > Cl(TS)
    & Cl(TS)>Op(LAGTS) ), TS)
  colnames(DarkCloudCover) <- c("DarkCloudCover")
  xtsAttributes(DarkCloudCover) <- list(bars=2)
  return(DarkCloudCover)
}