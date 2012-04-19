\name{CSPLongCandle}
\alias{CSPLongCandle}
\alias{CSPLongCandleBody}
\alias{CSPShortCandle}
\alias{CSPShortCandleBody}
\alias{LongCandle}
\alias{LongCandleBody}
\alias{ShortCandle}
\alias{ShortCandleBody}
\title{Long/Short Candlestick (Body) Pattern}
\description{Look for long/short candlesticks in a OHLC price series}
\usage{
CSPLongCandle(TS, n=20, threshold=1)
CSPShortCandle(TS, n=20, threshold=1)
CSPLongCandleBody(TS, n=20, threshold=1)
CSPShortCandleBody(TS, n=20, threshold=1)}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
  \item{n}{number of preceding candles to calculate median candle length}
  \item{threshold}{minimum/maximum candle length in relation to the median candle length of \code{n} preceding candles}
}
\details{
In order to classify a candle a long candle, the relative length \code{(Hi-Lo)/(Hi+Lo)/2} must be greater or equal \code{threshold} times the median of the relative candle length of \code{n} preceding candles.\cr
To classify a candle a short candle, the relative length must be smaller than \code{threshold} times the median of the relative candle length of \code{n} preceding candles.
}
\value{
  A xts object containing the columns:\cr
  \emph{CSPLongCandle:}
  \item{LongWhiteCandle}{TRUE if Long White Candle detected}
  \item{LongBlackCandle}{TRUE if Long Black Candle detected}
  \emph{CSPShortCandle:}
  \item{ShortWhiteCandle}{TRUE if Short White Candle detected}
  \item{ShortBlackCandle}{TRUE if Short Black Candle detected}
  \emph{CSPLongCandleBody:}
  \item{LongWhiteCandleBody}{TRUE if Long White Candle Body detected}
  \item{LongBlackCandleBody}{TRUE if Long Black Candle Body detected}
  \emph{CSPShortCandleBody:}
  \item{ShortWhiteCandleBody}{TRUE if Short White Candle Body detected}
  \item{ShortBlackCandleBody}{TRUE if Short Black Candle Body detected}
}
\author{Andreas Voellenklee}
\references{}
\note{}
\seealso{\code{\link{CandleLength}}, \code{\link{CSPNLongCandles}}}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPLongCandle(YHOO)
  CSPShortCandle(YHOO, threshold=.5) # filter for very small candles
  CSPLongCandleBody(YHOO)
  CSPShortCandleBody(YHOO)
}
}
\keyword{}
