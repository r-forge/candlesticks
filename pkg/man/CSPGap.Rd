\name{CSPGap}
\alias{CSPGap}
\alias{Gap}
\title{Gap Candlestick Pattern}
\description{Look for price gaps between two candles in a OHLC price series}
\usage{CSPGap(TS, ignoreShadows=FALSE)}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
  \item{ignoreShadows}{if TRUE, only Open and Close Price are evaluated. Hi and Low are ignored.}
}
\details{
  Number of candle lines: \bold{2}\cr
  When ignoreShadows=FALSE a gap up occurs when the candle's low is higher than the precedent candles's high.\cr
  When ignoreShadows=TRUE a gap up occurs when the candle's open is higher than the precedent candles's body.
}
\value{
  A xts object containing the columns:
  \item{GapUp}{TRUE if gap up detected}
  \item{GapDown}{TRUE if gap down detected}
}
\author{Andreas Voellenklee}
\references{}
\note{}
\seealso{}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPGap(YHOO) # examine whole candle lenght
  CSPGap(YHOO, ignoreShadows=TRUE) # examine only candle bodies
}
}
\keyword{}
