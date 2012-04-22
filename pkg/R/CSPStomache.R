CSPStomache <- function(TS) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  LAGTS <- LagOC(TS, k=1)
  AboveTheStomache <- reclass( Op(LAGTS)>Cl(LAGTS) & Cl(TS)>Op(TS)
    & Op(TS)>=((Op(LAGTS)+Cl(LAGTS))/2) , TS)
  BelowTheStomache <- reclass( Cl(LAGTS)>Op(LAGTS) & Op(TS)>Cl(TS)
    & ((Op(LAGTS)+Cl(LAGTS))/2>=Op(TS)) , TS)
  result <- cbind(AboveTheStomache, BelowTheStomache)
  colnames(result) <- c("AboveTheStomache", "BelowTheStomache")
  xtsAttributes(result) <- list(bars=2)
  return(result)
}
