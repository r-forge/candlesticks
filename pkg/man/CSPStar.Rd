\name{CSPStar}
\alias{CSPStar}
\alias{MorningStar}
\alias{EveningStar}
\title{Morning/Evening Star Candlestick Pattern}
\description{Look for Morning/Evening Star Patterns in a OHLC price series}
\usage{
CSPStar(TS, n=20, minbodysizeMedian=1, maxbodysizeMedian=1)
}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
  \item{n}{number of preceding candles to calculate median candle length}
  \item{minbodysizeMedian}{minimum candle length in relation to the median candle length of the formation's \emph{first} candle}
  \item{maxbodysizeMedian}{maximum candle length in relation to the median candle length of the formation's \emph{second} candle}
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
\note{The function filters patterns that look like morning/evening stars, without considering the current trend direction. If only pattern in uptrends should be filtered, a external trend detection function must be used. See examples.}
\seealso{
 \code{\link{CSPLongCandleBody}}
}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPStar(YHOO)
  
  # allow only a small second candle body
  CSPStar(YHOO, maxbodysizeMedian=.5)
  
  # filter for morning stars that occour in downtrends
  MorningStar <- (CSPStar(YHOO)[,"MorningStar"] & 
    TrendDetectionChannel(lag(YHOO,k=3))[,"DownTrend"]
}
}
