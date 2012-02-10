\name{CSPStar}
\alias{CSPStar}
\alias{MorningStar}
\alias{EveningStar}
\title{Morning/Evening Star Candlestick Pattern}
\description{Look for Morning/Evening Star Patterns in a OHLC price series}
\usage{
CSPStar(TS, n=20, lthreshold=1.5, sthreshold=1)
}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
  \item{n}{number of preceding candles to calculate median candle length}
  \item{lthreshold}{minimum candle length in relation to the median candle length of the formation's \emph{first} candle}
  \item{sthreshold}{maximum candle length in relation to the median candle length of the formation's \emph{second} candle}
}
\details{
  Number of candle lines: \bold{3}\cr\cr
  \emph{Morning Star:}\cr
   The market is in downtrend. The first candlestick is a long black body; the second one is a small real body of either color. It is characteristically marked with a gap in lower direction thus forming a star. Finally we see the black candlestick with a closing price well within first session's black real body. This pattern clearly shows that the market now turned bullish.\cr\cr
  \emph{Evening Star:}\cr
   The market is in uptrend. The first candlestick is a long white body; the second one is a small real body of either color. It is characteristically marked with a gap in higher direction thus forming a star. Finally we see the black candlestick with a closing price well within first session's white real body. This pattern clearly shows that the market now turned bearish.
}
\value{
  A xts object containing the columns:
  \item{MorningStar}{TRUE if Morning Star pattern detected}
  \item{EveningStar}{TRUE if Evening Star pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{http://www.candlesticker.com/Bullish.asp}\cr
  \url{http://www.candlesticker.com/Bearish.asp}
}
\note{Trend detection prior to the formation is not implemented yet}
\seealso{}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPStar(YHOO)
  CSPStar(YHOO, sthreshold=.5) # allow only a small second candle body
}
}
\keyword{}
