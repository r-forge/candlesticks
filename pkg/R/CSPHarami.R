CSPHarami <- function(TS, excludeDoji=FALSE, DojiBLRatio=.1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LAGTS <- LagOHLC(TS, k=1)
  BullHarami <- reclass(eval( 
    Op(LAGTS)>Cl(LAGTS) & Cl(TS)>Op(TS)
    & Op(LAGTS)>Cl(TS) & Cl(LAGTS)<Op(TS) 
    & Hi(LAGTS)>=Hi(TS) & Lo(LAGTS)<=Lo(TS) ), TS)
  BearHarami <- reclass(eval( 
    Cl(LAGTS)>Op(LAGTS) & Op(TS)>Cl(TS) 
    & Cl(LAGTS)>Op(TS) & Op(LAGTS)<Cl(TS) 
    & Hi(LAGTS)>=Hi(TS) & Lo(LAGTS)<=Lo(TS) ), TS)
  # some don't accept the second candle being a doji
  if (excludeDoji==TRUE) {
    Doji <- CSPDoji(TS, DojiBLRatio)
    BullHarami <- reclass(eval(BullHarami & !Doji[,1]), TS)
    BearHarami <- reclass(eval(BearHarami & !Doji[,1]), TS)
  }
  result <- cbind(BullHarami, BearHarami)
  colnames(result) <- c("Bull.Harami", "Bear.Harami")
  xtsAttributes(result) <- list(bars=2)
  return(result)
}
