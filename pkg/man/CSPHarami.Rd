\name{CSPHarami}
\alias{CSPHarami}
\alias{Harami}
\title{Harami Candlestick Pattern}
\description{Look for Bullish/Bearish Harami Pattern in a OHLC price series}
\usage{CSPHarami(TS, n=20, minbodysizeMedian=1)}
\arguments{
  \item{TS}{xts Time Series containing Open and Close Prices}
  \item{n}{number of preceding candles to calculate median candle body length}
  \item{minbodysizeMedian}{Minimum body length of the first candle (mother candle) relative to the median of the past \code{n} body sizes}
}
\details{
  Number of candle lines: \bold{2}\cr\cr
  \emph{Bullish Harami Pattern:}\cr
A candlestick chart pattern in which a large candlestick is followed by a smaller candlestick whose body is located within the vertical range of the larger body. In terms of candlestick colors, the bullish harami is a downtrend of black candlesticks engulfing a small positive white candlestick, giving a sign of a reversal of the downward trend.\cr\cr
  \emph{Bearish Harami Pattern:}\cr
Opposite of Bullish Harami Pattern.\cr\cr
}
\value{
  A xts object containing the columns:
  \item{Bull.Harami}{TRUE if bullish harami pattern detected}
  \item{Bear.Harami}{TRUE if bearish harami pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this candlestick pattern:\cr
  \url{http://www.candlesticker.com/Bullish.asp}\cr
  \url{http://www.candlesticker.com/Bearish.asp}
}
\note{The function filters patterns that look like haramis, without considering the current trend direction. If only pattern in uptrends should be filtered, a external trend detection function must be used. See examples.}
\seealso{
 \code{\link{CSPLongCandleBody}}
}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPHarami(YHOO)
  
  # filter for bullish harami that occour in downtrends
  BullHarami <- eval(CSPHarami(YHOO)[,"Bull.Harami"] & 
    TrendDetectionChannel(lag(YHOO,k=2))[,"DownTrend"])
}
}
\keyword{}
