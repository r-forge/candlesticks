# DonchianChannel function in TTR lacks a 1 period-lag of the result
# hence, this modified (corrected) version is used in package candlesticks
DonchianChannel2 <- function(HL, n = 10) {
    if (NCOL(HL) > 1 && (!has.Lo(HL) || !has.Hi(HL))) {
      stop("Price series must either contain High and Low, or be univariate.")
    }
    if (NCOL(HL) > 1) {
      hi <- Hi(HL)[, 1]
      lo <- Lo(HL)[, 1]
    } else hi <- lo <- HL
    high <- runMax(hi, n)
    low <- runMin(lo, n)
    mid <- (high + low)/2
    result <- lag(cbind(high, mid, low))
    colnames(result) <- c("high", "mid", "low")
    return(result)
  }