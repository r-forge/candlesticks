PriceLocation <- function(TS, n=252, sectors=3) {
  if (!has.Cl(TS)) {
    stop("Price series must contain Close prices")
  }
  DC <- DonchianChannel(Cl(TS), n=n)
  pos <- (Cl(TS)-DC[,'low'])/(DC[,'high']-DC[,'low'])
  sector <- floor(pos*sectors*.9999999999999)
  result <- cbind(pos,sector)
  colnames(result) <- c("pos", "sector")
  return(result)
}