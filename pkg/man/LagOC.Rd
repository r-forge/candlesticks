\name{LagOC}
\alias{LagOC}
\title{Lag a Open/Close Time Series}
\description{Create a lagged series from Open and Close price data}
\usage{LagOC(TS, k=1)}
\arguments{
  \item{TS}{xts Time Series containing Open, High, Low and Close Prices}
  \item{k}{periods to lag}
}
\details{
  shift series k-periods down, prepending NAs to front of series
}
\value{
  A xts object containing lagged Open and Close columns. The original column names are appended with \code{'.Lag.k'}, with \code{k} being the periods to lag.

  The returned series maintains the number of observations of the original.
}
\note{
  This function uses the Lag function of the \pkg{quantmod} package.
}
\seealso{
  \code{\link{Lag}}
  \code{\link{LagOHLC}}
}
\examples{
\dontrun{
getSymbols('YHOO')
LagOC(YHOO)        # Lag OC series by one period
LagOC(YHOO, k=1:3) # Lag OC series by one, two and three periods
                   # this will return a 6 columns xts object
}
}
\author{Andreas Voellenklee}
