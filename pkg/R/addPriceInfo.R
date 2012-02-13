addPriceInfo <- function (TS, CSP) {
  attr <- xtsAttributes(CSP)
  if (!is.numeric(attr$bars) | attr$bars <1) {
    stop("invalid xts Attribute 'bars'")
  }
  
  multiplicator <- reclass(as.xts(apply(CSP,1,max)), TS)
  BLEND <- CSPNBlended(TS, N=attr$bars)
  PINFO <- cbind(BLEND[,1]* multiplicator, 
                 BLEND[,2]* multiplicator, 
                 BLEND[,3]* multiplicator, 
                 BLEND[,4]* multiplicator)
  
  colnames(PINFO) <- c("Formation.Open", "Formation.High", "Formation.Low", "Formation.Close")
  result <- cbind(CSP, PINFO)
  return (result)
}