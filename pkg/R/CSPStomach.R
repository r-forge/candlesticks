CSPStomach <- function(TS) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  LAGTS <- LagOC(TS, k=1)
  AboveTheStomach <- reclass( Op(LAGTS)>Cl(LAGTS) & Cl(TS)>Op(TS)
    & Op(TS)>=((Op(LAGTS)+Cl(LAGTS))/2) , TS)
  BelowTheStomach <- reclass( Cl(LAGTS)>Op(LAGTS) & Op(TS)>Cl(TS)
    & ((Op(LAGTS)+Cl(LAGTS))/2>=Op(TS)) , TS)
  result <- cbind(AboveTheStomach, BelowTheStomach)
  colnames(result) <- c("AboveTheStomach", "BelowTheStomach")
  xtsAttributes(result) <- list(bars=2)
  return(result)
}
