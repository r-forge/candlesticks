CSPNHigherClose <- function (TS, N) {
  if (!has.Cl(TS)) {
    stop("Price series must contain Close prices")
  }
  if (N<1) {
    stop("N has to be a integer >= 1")
  }
  LAGTS <- LagOC(TS,k=0:N)
  result <- eval (Cl(LAGTS)[,1] > Cl(LAGTS)[,2])
  i <- 2
  while (i < N+1) {
    result <- eval (result & (Cl(LAGTS)[,i] > Cl(LAGTS)[,(i+1)]))
    i <- i+1
  }
  colnames(result) <- paste(N, "HigherClose", sep="")
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
  result <- eval (Cl(LAGTS)[,1] < Cl(LAGTS)[,2])
  i <- 2
  while (i < N+1) {
    result <- eval (result & (Cl(LAGTS)[,i] < Cl(LAGTS)[,(i+1)]))
    i <- i+1
  }
  colnames(result) <- paste(N, "LowerClose", sep="")
  return (result)
}