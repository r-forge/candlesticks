\name{CSPThreeLineStrike}
\alias{CSPThreeLineStrike}
\alias{ThreeLineStrike}
\title{Three Line Strike Candlestick Pattern}
\description{Look for bullish and bearish Three Line Strike Patterns in a Open/Close price series}
\usage{
  CSPThreeLineStrike(TS, n=25,  minbodysizeMedian=.5)
}
\arguments{
  \item{TS}{xts Time Series containing Open and Close prices}
  \item{n}{number of preceding candles to calculate median candle length}
  \item{minbodysizeMedian}{minimum candle length in relation to the median candle length of \code{n} preceding candles}  
}
\details{
  Number of candle lines: \bold{4}\cr
  
  For the bullish Three Line Strike, the first three candles must all be white with not too small candle bodies. The close of the second candle is above the close of the first. The close of the third candle is above the close of the second. The open of the last candle is at or above the close of the third candle, then in the same period the price moves down heavily and closes at or below the open of the first candle.\cr
  
  The bearish Three Line Strike is the opposite, with three black candles followed by a long white candle.\cr
  
  This formation is very rare. 
}
\value{
  A xts object containing the column:
  \item{Bull.ThreeLineStrike}{TRUE if bullish Three Line Strike pattern detected}
  \item{Bear.ThreeLineStrike}{TRUE if bearish Three Line Strike pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{https://hitandruncandlesticks.com/bullish-three-line-strike/}\cr
  \url{https://hitandruncandlesticks.com/bearish-three-line-strike/}\cr
  \url{http://thepatternsite.com/ThreeLineStrikeBull.html}
}
\note{The function filters patterns that look like three line strikes, without considering the current trend direction. If only pattern in uptrends or downtrends should be filtered, a external trend detection function must be used. See examples.}
\seealso{
  \code{\link{CSPNLongWhiteCandleBodies}}
  \code{\link{CSPNLongBlackCandleBodies}}
}
\examples{
\dontrun{
  Sys.setenv(TZ="UTC")

  getSymbols('COG')
  TLS <- CSPThreeLineStrike(COG)
  
  # how often does that occur?
  colSums(TLS, na.rm=TRUE)
  
  # when did that occur?
  TLS[TLS[,1]>0 | TLS[,2]>0,]
  
  # filter for bearish three line strikes that occur in downtrends 
  TLS1 <- CSPThreeLineStrike(COG)[,"Bear.ThreeLineStrike"] & 
    TrendDetectionChannel(lag(COG,k=4))[,"DownTrend"]
  TLS1[TLS1[,1]>0,]
  
  # show in a chart
  chartSeries(COG["2014-09/2014-10"])
}
}
