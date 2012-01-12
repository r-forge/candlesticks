\name{CSPDarkCloudCover}
\alias{CSPDarkCloudCover}
\alias{DarkCloudCover}
\title{Dark Cloud Cover Candlestick Pattern}
\description{Look for Dark Cloud Covers in a Open/Close price series}
\usage{CSPDarkCloudCover(TS)}
\arguments{
  \item{TS}{xts Time Series containing Open and Close Prices}
}
\details{
A white candlestick is followed by a gap higher during the next day while the market is in uptrend. The day ends up as a black candlestick, which closes more than halfway into the prior black candlestick’s real body.
}
\value{
  A xts object containing the column:
  \item{DarkCloudCover}{TRUE if Dark Cloud Cover detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{http://www.candlesticker.com/Bearish.asp}\cr
  \url{http://www.onlinetradingconcepts.com/TechnicalAnalysis/Candlesticks/DarkCloudCover.html}
}
\note{Trend detection prior to the formation is not implemented yet}
\seealso{The counterpart of this pattern is \code{\link{PiercingPattern}}}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPDarkCloudCover(YHOO)
}
}
\keyword{}
