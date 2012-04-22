\name{LagOHLC}
\alias{LagOHLC}
\title{Lag a OHLC Time Series}
\description{Create a lagged series from OHLC price data}
\usage{LagOHLC(TS, k=1)}
\arguments{
  \item{TS}{xts Time Series containing Open, High, Low and Close Prices}
  \item{k}{periods to lag}
}
\details{
  shift series k-periods down, prepending NAs to front of series
}
\value{
  A xts object containing lagged OHLC columns. The original column names are appended with \code{'.Lag.k'}, with \code{k} being the periods to lag.

  The returned series maintains the number of observations of the original.
}
\note{
  This function uses the Lag function of the \pkg{quantmod} package.
}
\seealso{
  \code{\link{Lag}}
  \code{\link{LagOC}}
}
\examples{
\dontrun{
  getSymbols('YHOO')
  LagOHLC(YHOO)        # Lag OHLC series by one period
  LagOHLC(YHOO, k=1:3) # Lag OHLC series by one, two and three periods
                       # this will return a 12 columns xts object
}
}
\author{Andreas Voellenklee}
\keyword{}
