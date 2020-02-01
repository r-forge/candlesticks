\name{CSPThreeMethods}
\alias{CSPThreeMethods}
\alias{RisingThreeMethods}
\alias{FallingThreeMethods}
\title{Rising/Falling Three Methods Candlestick Pattern}
\description{Look for Rising/Falling Three Methods Pattern in a OHLC price series}
\usage{CSPThreeMethods(TS, n=20, threshold=1.5)}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}  
  \item{n}{number of preceding candles to calculate median candle body length}
  \item{threshold}{minimum/maximum candle body length in relation to the median candle length of \code{n} preceding candle bodies}
}
\details{
Number of candle lines: \bold{5}\cr\cr
\emph{Rising Three Methods:}\cr
Prior trend: \bold{up}\cr
The formation starts with a long white candle body. The following three bars are smaller candles that open/close within the high/low span of the first candle. The last candle is a again a long white candle that closes above the close price of the first candle.
\cr\cr
\emph{Falling Three Methods:}\cr
Prior trend: \bold{down}\cr
The formation starts with a long black candle body. The following three bars are smaller candles that open/close within the high/low span of the first candle. The last candle is a again a long black candle that closes below the close price of the first candle.
}
\value{
  A xts object containing the columns:
  \item{RisingThreeMethods}{TRUE if Rising Three Methods pattern detected}
  \item{FallingThreeMethods}{TRUE if Falling Three Methods pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this candlestick pattern:\cr
  \url{http://www.investopedia.com/terms/r/rising-three-methods.asp}\cr
  \url{http://www.investopedia.com/terms/f/falling-three-methods.asp}\cr
  \url{http://www.candlesticker.com/Bullish.asp}\cr
  \url{http://www.candlesticker.com/Bearish.asp}
}
\note{The function filters patterns that look like three methods, without considering the current trend direction. If only pattern in uptrends should be filtered, a external trend detection function must be used. See examples.}
\examples{
\dontrun{
  getSymbols('URZ',adjust=TRUE)
  CSPThreeMethods(URZ)
  
  # filter rising three methods in uptrends
  CSPThreeMethods(URZ)[,"RisingThreeMethods"] & TrendDetectionChannel(lag(URZ,k=5))[,"UpTrend"]
}
}
