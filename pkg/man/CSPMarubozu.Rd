\name{CSPMarubozu}
\alias{CSPMarubozu}
\alias{Marubozu}
\title{Marubozu Candlestick Pattern}
\description{Look for Marubozu Candlestick Patterns in a OHLC price series}
\usage{CSPMarubozu(TS, n=20, threshold=1.5)}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
  \item{n}{number of preceding candles to calculate median candle length}
  \item{threshold}{minimum candle length in relation to the median candle length of \code{n} preceding candles}
}
\details{
Number of candle lines: \bold{1}\cr\cr
\emph{White Marubozu:}\cr
A long white candle that has no shadows on either end. Op=Lo, Cl=Hi.\cr\cr
\emph{White Opening Marubozu:}\cr
A long white candle body with an upper shadow but no lower shadow. Op=Lo.\cr\cr
\emph{White Closing Marubozu:}\cr
A long white candle body with a lower shadow but no upper shadow. Cl=Hi.\cr\cr
\emph{Black Marubozu:}\cr
A long black candle that has no shadows on either end. Op=Hi, Cl=Lo.\cr\cr
\emph{Black Opening Marubozu:}\cr
A long black candle body with a lower shadow but no upper shadow. Op=Hi.\cr\cr
\emph{Black Closing Marubozu:}\cr
A long black candle body with a upper shadow but no lower shadow. Cl=Lo.\cr\cr
}
\value{
  A xts object containing the columns:
  \item{WhiteMarubozu}{TRUE if pattern detected}
  \item{WhiteOpeningMarubozu}{TRUE if pattern detected}
  \item{WhiteClosingMarubozu}{TRUE if pattern detected}
  \item{BlackMarubozu}{TRUE if pattern detected}
  \item{BlackOpeningMarubozu}{TRUE if pattern detected}
  \item{BlackClosingMarubozu}{TRUE if pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{http://www.candlesticker.com/Bullish.asp}\cr
  \url{http://www.candlesticker.com/Bearish.asp}
}
\note{}
\seealso{\code{\link{CSPLongCandle}}}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPMarubozu(YHOO)
  CSPMarubozu(YHOO, threshold=2) # filter for very long marubozus
}
}
\keyword{}
