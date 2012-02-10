\name{CSPPiercingPattern}
\alias{CSPPiercingPattern}
\alias{PiercingPattern}
\title{Piercing Candlestick Pattern}
\description{Look for Piercing Patterns in a Open/Close price series}
\usage{CSPPiercingPattern(TS)}
\arguments{
  \item{TS}{xts Time Series containing Open and Close Prices}
}
\details{
A black candlestick is followed by a gap lower during the next day while the market is in downtrend. The day ends up as a white candlestick, which closes more than halfway into the prior black candlestick's real body.
}
\value{
  A xts object containing the column:
  \item{PiercingPattern}{TRUE if Piercing Pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{http://www.candlesticker.com/Bullish.asp}\cr
  \url{http://www.onlinetradingconcepts.com/TechnicalAnalysis/Candlesticks/PiercingPattern.html}
}
\note{Trend detection prior to the formation is not implemented yet}
\seealso{The counterpart of this pattern is \code{\link{DarkCloudCover}}}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPPiercingPattern(YHOO)
}
}
\keyword{}
