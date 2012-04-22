\name{CSPDarkCloudCover}
\alias{CSPDarkCloudCover}
\alias{DarkCloudCover}
\title{Dark Cloud Cover Candlestick Pattern}
\description{Look for Dark Cloud Covers in a Open/Close price series}
\usage{CSPDarkCloudCover(TS, n=20, minbodysizeMedian=1)}
\arguments{
  \item{TS}{xts Time Series containing Open and Close prices}
  \item{n}{number of preceding candles to calculate median candle body length}
  \item{minbodysizeMedian}{Minimum body length relative to the median of the past \code{n} body sizes}
}
\details{
Number of candle lines: \bold{2}\cr\cr
A long white candlestick is followed by a gap higher during the next day while the market is in uptrend. The day ends up as a black candlestick, which closes more than halfway into the prior black candlestick's real body.
}
\value{
  A xts object containing the column:
  \item{DarkCloudCover}{TRUE if Dark Cloud Cover detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{http://www.candlesticker.com/Bearish.asp}\cr
  \url{http://www.onlinetradingconcepts.com/TechnicalAnalysis/Candlesticks/DarkCloudCover.html}
}
\note{The function filters patterns that look like dark cloud covers, without considering the current trend direction. If only pattern in uptrends should be filtered, a external trend detection function must be used. See examples.}
\seealso{The counterpart of this pattern is \code{\link{PiercingPattern}}}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPDarkCloudCover(YHOO)
  
  # filter dark cloud covers that occur in uptrends.
  # the lag of 2 periods of the time series for trend detection
  # ensures that the uptrend is active \emph{before} the
  # dark cloud cover occurs.
  CSPDarkCloudCover(YHOO) & TrendDetectionChannel(lag(YHOO,k=2))[,"UpTrend"]
}
}
\keyword{}
