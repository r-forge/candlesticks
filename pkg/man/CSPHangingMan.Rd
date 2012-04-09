\name{CSPHangingMan}
\alias{CSPHangingMan}
\alias{HangingMan}
\title{Hanging Man Candlestick Pattern}
\description{Look for Hanging Man Pattern in a OHLC price series}
\usage{CSPHangingMan(TS, minuppershadowCL=2/3, maxlowershadowCL=.1, minbodyCL=.1)}
\arguments{
  \item{TS}{xts Time Series containing Open, High, Low and Close Prices}
  \item{minuppershadowCL}{minimum upper shadow to candle length ratio}
  \item{maxlowershadowCL}{maximum tolerated lower shadow to candle length ratio}
  \item{minbodyCL}{minimum body to candle length ratio}
}
\details{
Number of candle lines: \bold{1}\cr\cr
The hanging man is a one-day formation. This patter is expected to be a early sign for the reversal of a uptrend into an downtrend. It has got a long upper shadow, a small body at the bottom of the candle, and no or only a very short lower shadow. The color of the body is not important. The counterpart of this pattern is \code{\link{Hammer}}.
}
\value{
  A xts object containing the column:
  \item{HangingMan}{TRUE if hammer pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this candlestick pattern:\cr
  \url{http://www.candlesticker.com/Cs26.asp}\cr
}
\note{The function filters candles that look like hanging mans, without considering the current trend direction. If only hanging man patterns in a uptrend should be filtered, a external trend detection function must be used. See examples.}
\seealso{
\code{\link{CSPDoji}}
\code{\link{CSPHammer}}
\code{\link{TrendDetectionChannel}}
\code{\link{TrendDetectionSMA}}
}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  
  # filter for hanging man patterns
  CSPHangingMan(YHOO)
  
  # filter for hanging man patterns that occur in uptrends
  eval(CSPHangingMan(YHOO) & TrendDetectionChannel(YHOO)[,"UpTrend"])
}
}
\keyword{}
