\name{CSPEngulfing}
\alias{CSPEngulfing}
\alias{Engulfing}
\title{Engulfing Candlestick Pattern}
\description{Look for Bullish/Bearish Engulfing Pattern in a Open/Close price series}
\usage{CSPEngulfing(TS)}
\arguments{
  \item{TS}{xts Time Series containing Open and Close Prices}
}
\details{
  Number of candle lines: \bold{2}\cr\cr
\emph{Bullish Engulfing Pattern:}\cr
Bullish Engulfing Pattern is a pattern characterized by a large white real body engulfing a preceding small black real body, which appears during a downtrend. The white body does not necessarily engulf the shadows of the black body but totally engulfs the body itself.\cr\cr
\emph{Bearish Engulfing Pattern:}\cr
Bearish Engulfing Pattern is a large black real body, which engulfs a small white real body in an uptrend. The black body does not necessarily engulf the shadows of the white body but totally engulfs the body itself.
}
\value{
  A xts object containing the columns:
  \item{Bull.Engulfing}{TRUE if bullish engulfing pattern detected}
  \item{Bear.Engulfing}{TRUE if bearish engulfing pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this candlestick pattern:\cr
  \url{http://thepatternsite.com/BullEngulfing.html}\cr
  \url{http://www.onlinetradingconcepts.com/TechnicalAnalysis/Candlesticks/BullishEngulfing.html}\cr
  \url{http://www.candlesticker.com/Bullish.asp}\cr
  \url{http://www.candlesticker.com/Bearish.asp}
}
\note{}
\seealso{}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPEngulfing(YHOO)
}
}
\keyword{}
