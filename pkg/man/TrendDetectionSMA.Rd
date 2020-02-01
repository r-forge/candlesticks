\name{TrendDetectionSMA}
\alias{TrendDetectionSMA}
\title{Current Trend Detection using Simple Moving Average}
\description{Uses the SMA of \code{n} periods to determine whether a price series is in uptrend or downtrend}
\usage{TrendDetectionSMA(TS, n=20)}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
  \item{n}{number of periods for the SMA to average over}
}
\details{
This function assumes that a price series is in uptrend (downtrend) when a period's price closes above (below) the simple moving average of \code{n} periods.
}
\value{
  A xts object containing the columns:
  \item{UpTrend}{TRUE if uptrend detected (Close > SMA(n))}
  \item{NoTrend}{TRUE if sideward trend detected (Close == SMA(n))}
  \item{DownTrend}{TRUE if downtrend detected (Close < SMA(n))}
  \item{Trend}{+1 for uptrend, 0 for sideward trend, -1 for downtrend}
}
\author{Andreas Voellenklee}
\seealso{
  \code{\link{TrendDetectionChannel}}
}
\examples{
\dontrun{
  getSymbols("YHOO", adjust=TRUE)
  
  # create chart of YAHOO
  chartSeries(YHOO, subset="last 1 year", TA=NULL)
  
  # visualize the result of trend detection in a indicator box
  addTA(TrendDetectionSMA(YHOO)[,4])
  
  # filter YHOO for Hammer Candlestick Patterns that occur in downtrends
  Hammer <- CSPHammer(TS) & TrendDetectionSMA(TS)[,"DownTrend"]
  
  # how frequent are these hammers?
  colSums(Hammer, na.rm=TRUE)
}
}
