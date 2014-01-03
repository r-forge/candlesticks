CSPNBlended <- function (TS, N) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  
  if (N<1) {
    stop("N has to be a integer >= 1")
  }
  
  LAGTS <- LagOHLC(TS,k=0:(N-1))
  OP <- Op(LAGTS)[,N]
  HI <- reclass(as.xts(apply(Hi(LAGTS),1,max), dateFormat=indexClass(try.xts(TS)[1])), TS)
  LO <- reclass(as.xts(apply(Lo(LAGTS),1,min), dateFormat=indexClass(try.xts(TS)[1])), TS)
  CL <- Cl(LAGTS)[,1]
  result <- cbind(OP,HI,LO,CL)
  colnames(result) <- c(paste(N, ".Blended.Open", sep=""), 
                        paste(N, ".Blended.High", sep=""), 
                        paste(N, ".Blended.Low", sep=""), 
                        paste(N, ".Blended.Close", sep=""))
  return (result)
}