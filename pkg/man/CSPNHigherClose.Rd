\name{CSPNHigherClose}
\alias{CSPNHigherClose}
\alias{CSPNLowerClose}
\title{N consecutive higher/lower close prices}
\description{look for N consecutive higher/lower close prices}
\usage{
CSPNHigherClose(TS, N)
CSPNLowerClose(TS, N)}
\arguments{
  \item{TS}{xts Time Series containing Close prices}
  \item{N}{number of candles to test for higher/lower close series}
}
\details{
Only the close price is evaluated. The colors of the candles are ignored. 
}
\value{
  A xts object containing the column:
  \item{<N>HigherClose}{TRUE if current close is the \code{N}th higher close in a row}
  \item{<N>LowerClose}{TRUE if current close is the \code{N}th lower close in a row}
}
\author{Andreas Voellenklee}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPNHigherClose(YHOO, N=3) # filter for 3 consecutive higher close
  CSPNLowerClose(YHOO, N=4) # filter for 4 consecutive lower close
}
}
