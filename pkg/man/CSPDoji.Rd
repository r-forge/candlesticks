\name{CSPDoji}
\alias{CSPDoji}
\alias{Doji}
\title{Doji Candlestick Pattern}
\description{Look for Doji Pattern in a OHLC price series}
\usage{CSPDoji(TS, maxbodyCL=.1, maxshadowCL=.1)}
\arguments{
  \item{TS}{xts Time Series containing Open and Close Prices}
  \item{maxbodyCL}{maximum body to length ratio, default = .1}
  \item{maxshadowCL}{maximum tolerated upper (lower) shadow to identify a dragonfly (gravestone) doji}
}
\details{
Number of candle lines: \bold{1}\cr
A Doji is formed when the open and the close are the same or very close. By default settings, a Doji is identified when the body of the candle is smaller or equal 1/10 of its full lenght.\cr
The bullish Dragonfly Doji pattern has got a long lower shadow, but no upper shadow. In opposition, the bearish Gravestone Doji has got a long upper shadow but no lower shadow. Dragonfly and Gravestone Doji can be reversal signs during down/uptrends.
}
\value{
  A xts object containing the columns:
  \item{Doji}{TRUE if Doji pattern detected}
  \item{DragonflyDoji}{TRUE if Dragonfly Doji pattern detected}
  \item{GravestoneDoji}{TRUE if Gravestone Doji pattern detected}  
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this candlestick pattern:\cr
  \url{http://www.onlinetradingconcepts.com/TechnicalAnalysis/Candlesticks/Doji.html}\cr
  \url{http://www.candlesticker.com/Bullish.asp}\cr
  \url{http://www.candlesticker.com/Bearish.asp}
}
\note{The function filters candles that look like dojis, without considering the current trend direction. If only doji patterns in a uptrend should be filtered, a external trend detection function must be used. See examples.}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  
  CSPDoji(YHOO)
  
  # filter for doji patterns that have exactly equal 
  # open and close prices
  CSPDoji(YHOO, DojiBLRatio=0)
  
  # filter for gravestone doji patterns that occur in uptrends
  CSPDoji(YHOO)[,"GravestoneDoji"] & TrendDetectionChannel(YHOO)[,"UpTrend"]
}
}
