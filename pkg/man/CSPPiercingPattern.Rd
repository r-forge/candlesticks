\name{CSPPiercingPattern}
\alias{CSPPiercingPattern}
\alias{PiercingPattern}
\title{Piercing Candlestick Pattern}
\description{Look for Piercing Patterns in a Open/Close price series}
\usage{CSPPiercingPattern(TS, n=20, minbodysizeMedian=1)}
\arguments{
  \item{TS}{xts Time Series containing Open and Close Prices}
  \item{n}{number of preceding candles to calculate median candle body length}
  \item{minbodysizeMedian}{Minimum body length relative to the median of the past \code{n} body sizes}
}
\details{
Number of candle lines: \bold{2}\cr\cr
A long black candlestick is followed by a gap lower during the next day while the market is in downtrend. The day ends up as a white candlestick, which closes more than halfway into the prior black candlestick's real body.
}
\value{
  A xts object containing the column:
  \item{PiercingPattern}{TRUE if Piercing Pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{http://www.candlesticker.com/Bullish.asp}\cr
  \url{http://www.onlinetradingconcepts.com/TechnicalAnalysis/Candlesticks/PiercingPattern.html}
}
\note{The function filters patterns that look like piercing patterns, without considering the current trend direction. If only pattern in downtrends should be filtered, a external trend detection function must be used. See examples.}
\seealso{The counterpart of this pattern is \code{\link{DarkCloudCover}}}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPPiercingPattern(YHOO)
  
  # filter piercing patterns that occur in downtrends.
  # the lag of 2 periods of the time series for trend detection
  # ensures that the uptrend is active \emph{before} the
  # dark cloud cover occurs.
  eval(CSPPiercingPattern(YHOO) & TrendDetectionChannel(lag(YHOO,k=2))[,"DownTrend"])
}
}
\keyword{}
