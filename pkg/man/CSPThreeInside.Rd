\name{CSPThreeInside}
\alias{CSPThreeInside}
\alias{ThreeInsideUp}
\alias{ThreeInsideDown}
\title{Three Inside Up/Down Candlestick Pattern}
\description{Look for Three Inside Up/Down Pattern in a OHLC price series.}
\usage{
  CSPThreeInside(TS)
}
\arguments{
  \item{TS}{xts Time Series containing Open, High, Low and Close Prices}
}
\details{
  Number of candle lines: \bold{3}\cr\cr
  \emph{Three Inside Up:}\cr
The market is in downtrend. The first two days of this pattern is the Bullish Harami Pattern, and the third day confirms the reversal suggested by the Bullish Harami Pattern, since it is a white candlestick closing with a new high for the last three days.\cr\cr
  \emph{Three Inside Down:}\cr
The market is in uptrend. The first two days of this pattern is the Bearish Harami Pattern, and the third day confirms the reversal suggested by the Bearish Harami Pattern, since it is a black candlestick closing with a new low for the last three days. 
}
\value{
  A xts object containing the columns:
  \item{ThreeInsideUp}{TRUE if Three Inside Up pattern detected}
  \item{ThreeInsideDown}{TRUE if Three Inside Down pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this candlestick pattern:\cr
  \url{http://www.candlesticker.com/Bullish.asp}\cr
  \url{http://www.candlesticker.com/Bearish.asp}
}
\note{The function filters patterns that look like three inside up/down, without considering the current trend direction. If only pattern in downtrends should be filtered, a external trend detection function must be used. See examples.}
\seealso{
  \code{\link{CSPHarami}}
  \code{\link{CSPThreeOutside}}
}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPThreeInside(YHOO)
  
  # filter three inside up in downtrends
  CSPThreeInside(YHOO)[,"ThreeInsideUp"] & TrendDetectionChannel(lag(YHOO,k=3))[,"DownTrend"]
}
}
