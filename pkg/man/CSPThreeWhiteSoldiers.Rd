\name{CSPThreeWhiteSoldiers}
\alias{CSPThreeWhiteSoldiers}
\alias{ThreeWhiteSoldiers}
\title{Three White Soldiers Candlestick Pattern}
\description{Look for Three White Soldiers in a Open/Close price series}
\usage{
  CSPThreeWhiteSoldiers(TS, strict=TRUE, n=20, threshold=1.5)
}
\arguments{
  \item{TS}{xts Time Series containing Open and Close prices}
  \item{strict}{if FALSE, use less strict conditions to detect pattern. See details.}
  \item{n}{number of preceding candles to calculate median candle length}
  \item{threshold}{minimum candle length in relation to the median candle length of \code{n} preceding candles}  
}
\details{
  Number of candle lines: \bold{3}\cr\cr
  The market forms a downtrend. The pattern is characterized by three long candlesticks stepping upward like a staircase. In strict mode, the opening of each day is lower than the previous close but higher than the previous open. When strict=FALSE, candle 2 and/or 3 of the formation may open higher than the previous day's close, thus forming a gap.
}
\value{
  A xts object containing the column:
  \item{ThreeWhiteSoldiers}{TRUE if Three White Soldiers pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{http://www.candlesticker.com/Bullish.asp}\cr
  \url{http://www.candlesticker.com/Bearish.asp}
}
\note{Trend detection prior to the formation is not implemented yet}
\seealso{
  \code{\link{ThreeBlackCrows}}
  \code{\link{CSPNLongWhiteCandles}}
  \code{\link{CSPNLongWhiteCandleBodies}}
}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPThreeWhiteSoldiers(YHOO)
  CSPThreeWhiteSoldiers(YHOO, strict=FALSE)
}
}
\keyword{}
