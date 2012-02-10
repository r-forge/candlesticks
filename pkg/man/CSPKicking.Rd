\name{CSPKicking}
\alias{CSPKicking}
\alias{Kicking}
\title{Kicking Candlestick Pattern}
\description{Look for bullish/bearish Kicking Candlestick Patterns in a OHLC price series}
\usage{
  CSPKicking(TS, ignoreShadows=TRUE, n=20, threshold=1.5)
}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
  \item{ignoreShadows}{if TRUE, only Open and Close Price are evaluated. Hi and Low are ignored.}
  \item{n}{number of preceding candles to calculate median candle length}
  \item{threshold}{minimum candle length in relation to the median candle length of \code{n} preceding candles}
}
\details{
  Number of candle lines: \bold{2}\cr\cr
  \emph{Bullish Kicking Pattern:}\cr
  The Bullish Kicking Pattern is a White Marubozu following a Black Marubozu. After the Black Marubozu, market gaps higher on the opening and it opens with a gap above the prior session's opening thus forming a White Marubozu. This pattern is filtered with the option \code{ignoreShadows=FALSE}, and occurs very scarcely.\cr
  A less strict filter rule is when \code{ignoreShadows=TRUE}. In this case, the pattern is detected when a long black candle body follows a long white candle body, and the open of the first candle is lower than the open of the second candle.\cr\cr
  \emph{Bearish Kicking Pattern:}\cr
  Opposite of Bullish Kicking Pattern.
}
\value{
  A xts object containing the columns:
  \item{Bull.Kicking}{TRUE if Bullish Kicking pattern detected}
  \item{Bear.Kicking}{TRUE if Bearish Kicking pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{http://www.candlesticker.com/Bullish.asp}\cr
  \url{http://www.candlesticker.com/Bearish.asp}
}
\note{
  Unlike most other candlestick patterns, the previous market direction is not important for this pattern.
}
\seealso{
  \code{\link{CSPLongCandleBody}}
  \code{\link{CSPMarubozu}}
  \code{\link{CSPGap}}
}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  
  # look for Kicking Pattern right out of the textbook
  # they occur only once in a blue moon
  CSPKicking(YHOO, ignoreShadows=FALSE)
  
  # use less strict filter rules
  CSPKicking(YHOO)
}
}
\keyword{}
