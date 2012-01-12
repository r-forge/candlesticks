\name{CandleLength}
\alias{CandleLength}
\alias{CandleBodyLength}
\title{Candle (Body) Length}
\description{relative and absolute length of a price candle}
\usage{
  CandleLength(HLTS)
  CandleBodyLength(OCTS)}
\arguments{
  \item{HLTS}{xts Time Series containing High and Low Prices}
  \item{OCTS}{xts Time Series containing Open and Close Prices}
}
\details{
  Both a relative candle length \code{(Hi-Lo)/(Hi+Lo)/2} and the absolute length \code{(Hi-Lo)} is calculated.
}
\value{
  A xts object containing the columns:
  \item{relCandleLength}{candle length / midpoint of candle}
  \item{absCandleLength}{candle length from low to high}
}
\author{Andreas Voellenklee}
\references{}
\note{}
\seealso{\code{\link{CSPLongCandle}}}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CandleLength(YHOO)
  CandleBodyLength(YHOO)
}
}
\keyword{}
