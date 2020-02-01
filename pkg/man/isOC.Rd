\name{is.OC}
\alias{is.OC}
\alias{is.HL}
\title{Check for Open/Close High/Low Data}
\description{functions to check for appropriate OC / HL column names within a data object}
\usage{
  is.OC(x)
  is.HL(x)}
\arguments{
  \item{x}{data object}
}
\details{
  The candlestick package uses check for OHLC data functions from the package \pkg{quantmod}. The functions is.OC and is.HL are not included in quantmod, therefore added into \pkg{candlesticks}
}
\value{
  \item{is.OC}{TRUE, if x includes columns for Open and Close}
  \item{is.HL}{TRUE, if x includes columns for High and Low}
}
\seealso{
  \code{\link{has.OHLC}}
}
\examples{
\dontrun{
getSymbols('YHOO')
is.OC(YHOO)
is.HL(YHOO)
}
}
\author{Andreas Voellenklee}
