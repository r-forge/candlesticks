\name{TrendDetectionChannel}
\alias{TrendDetectionChannel}
\title{Current Trend Detection using Donchian Channel}
\description{Uses the Donchian Channel of \code{n} periods to determine whether a price series is in uptrend, sideward trend or downtrend}
\usage{TrendDetectionChannel(TS, n=20, DCSector=1/3)}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
  \item{n}{number of periods to calculate the high and low band of the Donchian Channel}
  \item{DCSector}{sector of Donchian Channel to determine uptrend / downtrend}
}
\details{
This function assumes that a price series is in uptrend when a period's price closes above the upper third of the Donchian Channel of \code{n} periods. If the price close below the lower third of the channel, a downtrend is detected. If the price closes within the middle third of the channel, a sideward trend is detected. The parameter DCSector can be used to widen/narrow the threshold of up/downtrend detection. E.g. a value of .25 anticipates the price for a uptrend to be above the highest \emph{quarter} of the donchian channel
}
\value{
  A xts object containing the columns:
  \item{UpTrend}{TRUE if uptrend detected}
  \item{NoTrend}{TRUE if sideward trend detected}
  \item{DownTrend}{TRUE if downtrend detected}
  \item{Trend}{+1 for uptrend, 0 for sideward trend, -1 for downtrend}
}
\author{Andreas Voellenklee}
\references{
}
\note{}
\seealso{\code{\link{TrendDetectionSMA}}}
\examples{
\dontrun{
  getSymbols("YHOO", adjust=TRUE)
  
  # create chart of YAHOO
  chartSeries(YHOO, subset="last 1 year", TA=NULL)
  
  # visualize the result of trend detection in a indicator box
  addTA(TrendDetectionChannel(YHOO)[,4])
  
  # filter YHOO for Hammer Candlestick Patterns that occur in downtrends
  Hammer <- CSPHammer(TS) & TrendDetectionChannel(TS)[,"DownTrend"]
  
  # how frequent are these hammers?
  colSums(Hammer, na.rm=TRUE)
}
}
\keyword{}
