CSPThreeLineStrike <- function(TS, n=25, minbodysizeMedian=.5) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  LAGTS <- LagOC(TS,k=0:3)  # lags 0, 1, 2, 3 periods
  Cl_LAGTS <- Cl(LAGTS)
  Op_LAGTS <- Op(LAGTS)
  THREELWCB <- CSPNLongWhiteCandleBodies(TS, N=3, n=n, threshold=minbodysizeMedian)
  LAGTHREELWCB <- stats::lag(THREELWCB, 1)
  THREELBCB <- CSPNLongBlackCandleBodies(TS, N=3, n=n, threshold=minbodysizeMedian)
  LAGTHREELBCB <- stats::lag(THREELBCB, 1)
  BullTLS <- reclass(  
    LAGTHREELWCB[,1] &              # first 3 candles are long and white 
    Cl_LAGTS[,3] > Cl_LAGTS[,4] &   # close of 2nd candle higher than close of 1st
    Cl_LAGTS[,2] > Cl_LAGTS[,3] &   # close of 3rd candle higher than close of 2nd
    Op_LAGTS[,1] >= Cl_LAGTS[,2] &  # open of last candle higher or equal than close of 3rd
    Cl_LAGTS[,1] <= Op_LAGTS[,4],   # close of last candle lower than opening of 1st
  TS)
  BearTLS <- reclass(  
    LAGTHREELBCB[,1] &                # first 3 candles are long and black 
      Cl_LAGTS[,3] < Cl_LAGTS[,4] &   # close of 2nd candle lower than close of 1st
      Cl_LAGTS[,2] < Cl_LAGTS[,3] &   # close of 3rd candle lower than close of 2nd
      Op_LAGTS[,1] <= Cl_LAGTS[,2] &  # open of last candle lower or equal than close of 3rd
      Cl_LAGTS[,1] >= Op_LAGTS[,4],   # close of last candle higher than opening of 1st
    TS)
  result <- cbind(BullTLS, BearTLS)
  colnames(result) <- c("Bull.ThreeLineStrike", "Bear.ThreeLineStrike")
  xtsAttributes(result) <- list(bars=4)
  return(result)
}
