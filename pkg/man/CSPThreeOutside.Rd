\name{CSPThreeOutside}
\alias{CSPThreeOutside}
\alias{ThreeOutsideUp}
\alias{ThreeOutsideDown}
\title{Three Outside Up/Down Candlestick Pattern}
\description{Look for Three Outside Up/Down Pattern in a Open/Close price series.}
\usage{
  CSPThreeOutside(TS)
}
\arguments{
  \item{TS}{xts Time Series containing Open and Close Prices}
}
\details{
  Number of candle lines: \bold{3}\cr\cr
  \emph{Three Outside Up:}\cr
The market is in downtrend. The first two days of this pattern is the Bullish Engulfing Pattern, and the third day confirms the reversal suggested by the Bullish Engulfing Pattern, since it is a white candlestick closing with a new high for the last three days.\cr\cr
  \emph{Three Outside Down:}\cr
The market is in uptrend. The first two days of this pattern is the Bearish Engulfing Pattern, and the third day confirms the reversal suggested by the Bearish Engulfing Pattern, since it is a black candlestick closing with a new low for the last three days. 
}
\value{
  A xts object containing the columns:
  \item{ThreeOutsideUp}{TRUE if Three Outside Up pattern detected}
  \item{ThreeOutsideDown}{TRUE if Three Outside Down pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this candlestick pattern:\cr
  \url{http://www.candlesticker.com/Bullish.asp}\cr
  \url{http://www.candlesticker.com/Bearish.asp}
}
\note{The function filters patterns that look like three outside up/down, without considering the current trend direction. If only pattern in downtrends should be filtered, a external trend detection function must be used. See examples.}
\seealso{
  \code{\link{CSPEngulfing}}
  \code{\link{CSPThreeInside}}
}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPThreeOutside(YHOO)
  
  # filter three outside up in downtrends
  CSPThreeOutside(YHOO)[,"ThreeInsideUp"] & TrendDetectionChannel(lag(YHOO,k=3))[,"DownTrend"]
}
}
