\name{CSPNLongWhiteCandles}
\alias{CSPNLongCandles}
\alias{CSPNLongWhiteCandles}
\alias{CSPNLongBlackCandles}
\alias{CSPNLongWhiteCandleBodies}
\alias{CSPNLongBlackCandleBodies}
\title{N consecutive long candles / bodies of same color}
\description{look for N consecutive long candles / long candle bodies of same color}
\usage{
CSPNLongWhiteCandles(TS, N=2, n=20, threshold=1.5)
CSPNLongBlackCandles(TS, N=2, n=20, threshold=1.5)
CSPNLongWhiteCandleBodies(TS, N=2, n=20, threshold=1.5)
CSPNLongBlackCandleBodies(TS, N=2, n=20, threshold=1.5)
}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
  \item{N}{number of candles to test}
  \item{n}{number of preceding candles to calculate median candle length}
  \item{threshold}{minimum/maximum candle length in relation to the median candle length of \code{n} preceding candles}
}
\details{}
\value{
  A xts object containing the column:
  \item{<N>LongWhiteCandles}{TRUE if current candle is the \code{N}-th consecutive long white candle}
  \item{<N>LongBlackCandles}{TRUE if current candle is the \code{N}-th consecutive long black candle}
  \item{<N>LongWhiteCandleBodies}{TRUE if current candle is the \code{N}-th consecutive long white candle body}
  \item{<N>LongBlackCandleBodies}{TRUE if current candle is the \code{N}-th consecutive long black candle body}
}
\author{Andreas Voellenklee}
\references{
}
\note{}
\seealso{
\code{\link{CSPLongCandle}}
\code{\link{CSPLongCandleBody}}
}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPNLongWhiteCandles(YHOO)
  CSPNLongBlackCandles(YHOO, N=3)  
  CSPNLongWhiteCandleBodies(YHOO)
  CSPNLongBlackCandleBodies(YHOO, N=4, n=50, threshold=1.2)
}
}
\keyword{}
