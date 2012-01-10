CloseToSMA <- function(TS, n=20) {
  if (!has.Cl(TS)) {
    stop("Price series must contain Close prices")
  }
  result <- Cl(TS)/SMA(Cl(TS),n=n)
  colnames(result) <- paste(colnames(Cl(TS)),"_SMA",n,sep="")
  return(result)
  # values >1 indicate close price above SMA line
}