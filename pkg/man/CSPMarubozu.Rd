\name{CSPMarubozu}
\alias{CSPMarubozu}
\alias{Marubozu}
\title{Marubozu Candlestick Pattern}
\description{Look for Marubozu Candlestick Patterns in a OHLC price series}
\usage{CSPMarubozu(TS, n=20, ATRFactor=1, maxuppershadowCL=.1, maxlowershadowCL=.1)}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
  \item{n}{number of preceding candles to calculate Average True Range}
  \item{ATRFactor}{minimum size of candle body compared to the ATR}
  \item{maxuppershadowCL}{maximum tolerated upper shadow to candle length ratio}
  \item{maxlowershadowCL}{maximum tolerated lower shadow to candle length ratio}
}
\details{
Number of candle lines: \bold{1}\cr\cr
\emph{White Marubozu:}\cr
A long white candle that has no shadows or only small shadows on either end.\cr\cr
\emph{Black Marubozu:}\cr
A long black candle that has no shadows or only small shadows on either end.\cr\cr
}
\value{
  A xts object containing the columns:
  \item{WhiteMarubozu}{TRUE if pattern detected}
  \item{BlackMarubozu}{TRUE if pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{http://www.candlesticker.com/Bullish.asp}\cr
  \url{http://www.candlesticker.com/Bearish.asp}
}
\note{
In default settings, the candle \emph{body} length must be greater than the average true range of last \code{n} periods. The threshold can be varied by \code{ATRFactor}.
}
\seealso{
\code{\link{CandleBodyLength}}
\code{\link{ATR}}
}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)

  CSPMarubozu(YHOO)
  
  # include not-so-long-marubozus
  CSPMarubozu(YHOO, ATRFactor=.8)
  
  # filter for white closing marubozus (Cl(TS)=Hi(TS))
  CSPMarubozu(YHOO, maxuppershadowCL=0)[,"WhiteMarubozu"]
}
}
