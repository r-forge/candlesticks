CSPNHigherClose <- function (TS, N) {
  if (!has.Cl(TS)) {
    stop("Price series must contain Close prices")
  }
  if (N<1) {
    stop("N has to be a integer >= 1")
  }
  LAGTS <- LagOC(TS,k=0:N)
  result <- reclass( Cl(LAGTS)[,1] > Cl(LAGTS)[,2] , TS)
  i <- 2
  while (i < N+1) {
    result <- reclass( result & (Cl(LAGTS)[,i] > Cl(LAGTS)[,(i+1)]) , TS)
    i <- i+1
  }
  colnames(result) <- paste(N, "HigherClose", sep="")
  xtsAttributes(result) <- list(bars=N)
  return (result)
}

CSPNLowerClose <- function (TS, N) {
  if (!has.Cl(TS)) {
    stop("Price series must contain Close prices")
  }
  if (N<1) {
    stop("N has to be a integer >= 1")
  }
  LAGTS <- LagOC(TS,k=0:N)
  result <- reclass( Cl(LAGTS)[,1] < Cl(LAGTS)[,2] , TS)
  i <- 2
  while (i < N+1) {
    result <- reclass( result & (Cl(LAGTS)[,i] < Cl(LAGTS)[,(i+1)]) , TS)
    i <- i+1
  }
  colnames(result) <- paste(N, "LowerClose", sep="")
  xtsAttributes(result) <- list(bars=N)
  return (result)
}