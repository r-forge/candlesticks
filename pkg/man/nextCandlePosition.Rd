\name{nextCandlePosition}
\alias{nextCandlePosition}
\title{Position and color of the next candle in a OC price series}
\description{Examines open and close price of the following candle relative to the close price of the current candle. Also returns the color of the folling candle.}
\usage{nextCandlePosition(TS)}
\arguments{
  \item{TS}{xts Time Series containing Open and Close Prices}
}
\details{
  Sometimes it is suggested to wait for a confirmation in the form of a higher/lower following close and/or a white/black candle before conisdering trading the pattern. This function compares the price levels and returns a xts object with columns of boolean values for each contition.
}
\value{
  A xts object containing the columns:
  \item{HigherOpen}{TRUE if following candle opens higher than current close}
  \item{LowerOpen}{TRUE if following candle opens lower than current close}
  \item{HigherClose}{TRUE if following candle closes higher than current close}
  \item{LowerClose}{TRUE if following candle closes lower than current close}
  \item{White}{TRUE if following candle is white (Cl>Op)}
  \item{Black}{TRUE if following candle is black (Cl<Op)}
}
\note{
  This function uses the Next function of the \pkg{quantmod} package.
}
\seealso{
  \code{\link{Next}}
}
\examples{
\dontrun{
# filter for inverted hammers in uptrends that is followed by a lower close the next day
getSymbols('YHOO', adjust=TRUE)
CSPInvertedHammer(YHOO) & 
  TrendDetectionChannel(YHOO)[,"UpTrend"] & 
  nextCandlePosition(YHOO)[,"LowerClose"]
}
}
\author{Andreas Voellenklee}
\keyword{}
