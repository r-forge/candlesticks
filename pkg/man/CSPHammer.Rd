\name{CSPHammer}
\alias{CSPHammer}
\alias{Hammer}
\title{Hammer Candlestick Pattern}
\description{Look for Hammer Pattern in a OHLC price series}
\usage{CSPHammer(TS, minlowershadowCL=2/3, maxuppershadowCL=.1, minbodyCL=.1)}
\arguments{
  \item{TS}{xts Time Series containing Open, High, Low and Close Prices}
  \item{minlowershadowCL}{minimum lower shadow to candle length ratio}
  \item{maxuppershadowCL}{maximum tolerated upper shadow to candle length ratio}
  \item{minbodyCL}{minimum body to candle length ratio}
}
\details{
Number of candle lines: \bold{1}\cr\cr
The hammer is a one-day formation. This patter is expected to be a early sign for the reversal of a downtrend into an uptrend. It has got a long lower shadow, a small body at the top of the candle, and no or only a very short upper shadow. The color of the body is not important.
}
\value{
  A xts object containing the column:
  \item{Hammer}{TRUE if hammer pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this candlestick pattern:\cr
  \url{http://www.candlesticker.com/Cs18.asp}\cr
}
\note{The function filters candles that look like hammers, without considering the current trend direction. If only hammer patterns in a downtrend should be filtered, a external trend detection function must be used. See examples.}
\seealso{
\code{\link{CSPDoji}}
\code{\link{TrendDetectionChannel}}
\code{\link{TrendDetectionSMA}}
}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  
  # filter for hammer patterns
  CSPHammer(YHOO)
  
  # filter for hammer patterns that occur in downtrends
  CSPHammer(YHOO) & TrendDetectionChannel(YHOO)[,"DownTrend"]
}
}
