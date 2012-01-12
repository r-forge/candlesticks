\name{CSPStomache}
\alias{CSPStomache}
\alias{Stomache}
\alias{AboveTheStomache}
\alias{BelowTheStomache}
\title{Above/Below The Stomache Candlestick Pattern}
\description{Look for Above/Below The Stomache Pattern in a Open/Close Time Series}
\usage{CSPStomache(TS)}
\arguments{
  \item{TS}{xts Time Series containing Open and Close Prices}
}
\details{
Number of candle lines: \bold{2}\cr\cr
\emph{Above The Stomache:}\cr
Prior trend: \bold{down}\cr
A black candle followed by a white candle. The opening price of the second day must be at or above the mid point of the first candle’s body.\cr\cr
\emph{Below The Stomache:}\cr
Prior trend: \bold{up}\cr
A white candle followed by a black candle. The opening price of the second day must be at or below the mid point of the first candle’s body.\cr
}
\value{
  A xts object containing the columns:
  \item{AboveTheStomache}{TRUE if Above The Stomache pattern detected}
  \item{BelowTheStomache}{TRUE if Below The Stomache pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{http://www.thepatternsite.com/AboveStomach.html}\cr
  \url{http://www.thepatternsite.com/BelowStomach.html}
}
\note{Trend detection prior to the formation is not implemented yet}
\seealso{}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPStomache(YHOO)
}
}
\keyword{}
