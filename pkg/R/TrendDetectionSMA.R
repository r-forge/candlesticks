TrendDetectionSMA<- function(TS, n=20) {
  if (!has.Cl(TS)) {
    stop("Price series must contain Close prices")
  }
  Close <- Cl(TS)
  CloseToSMA <- Close/SMA(Close,n=n)
  UpTrend <- eval(CloseToSMA[,1] > 1)
  NoTrend <- eval(CloseToSMA[,1] == 1)
  DownTrend <- eval(CloseToSMA[,1] < 1)
  Trend <- UpTrend+ DownTrend*(-1)
  result <- cbind(UpTrend, NoTrend, DownTrend, Trend)
  colnames(result) <- c("UpTrend", "NoTrend", "DownTrend", "Trend")
  return(result)
}