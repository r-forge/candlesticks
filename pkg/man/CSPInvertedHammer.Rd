\name{CSPInvertedHammer}
\alias{CSPInvertedHammer}
\alias{InvertedHammer}
\title{Inverted Hammer Candlestick Pattern}
\description{Look for Inverted Hammer Pattern in a OHLC price series}
\usage{CSPInvertedHammer(TS, minuppershadowCL=2/3, maxlowershadowCL=.1, minbodyCL=.1)}
\arguments{
  \item{TS}{xts Time Series containing Open, High, Low and Close Prices}
  \item{minuppershadowCL}{minimum upper shadow to candle length ratio}
  \item{maxlowershadowCL}{maximum tolerated lower shadow to candle length ratio}
  \item{minbodyCL}{minimum body to candle length ratio}
}
\details{
Number of candle lines: \bold{1}\cr\cr
The inverted hammer has got a long upper shadow, a small body at the bottom of the candle, and no or only a very short lower shadow.\code{\link{Hammer}}.
}
\value{
  A xts object containing the column:
  \item{InvertedHammer}{TRUE if inverted hammer pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this candlestick pattern:\cr
  \url{http://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:introduction_to_candlesticks}\cr
}
\note{The function filters candles that look like inverted hammers, without considering the current trend direction. If only inverted hammer patterns in a uptrend should be filtered, a external trend detection function must be used. See examples.}
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
  CSPInvertedHammer(YHOO)
  
  # filter for hanging man patterns that occur in downtrends
  CSPInvertedHammer(YHOO) & TrendDetectionChannel(YHOO)[,"DownTrend"]
}
}
\keyword{}
