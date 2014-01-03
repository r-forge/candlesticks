\name{candlesticks-package}
\alias{candlesticks-package}
\alias{candlesticks}
\docType{package}
\title{
Functions for Candlestick Pattern Recognition
}
\description{
This package contains functions to detect the most popular japanese candlestick formations like Doji, Engulfing Pattern, Harami, etc. out of OHLC price data.
}
\details{
\tabular{ll}{
Package: \tab candlesticks\cr
Version: \tab see description file\cr
Date: \tab see description file\cr
Depends: \tab R (>= 2.13), xts (>= 0.8-2), quantmod (>= 0.3-17), TTR (>= 0.21-0)\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
}
\author{
Andreas Voellenklee

Maintainer: Andreas Voellenklee <wotuzu17@gmail.com>
}
\references{
  The following sites were used to code/document this package:\cr
  \url{http://www.onlinetradingconcepts.com/TechnicalAnalysis/Candlesticks/CandlestickBasics.html}\cr
  \url{http://candlesticker.com/}\cr
  \url{http://thepatternsite.com/CandleEntry.html}\cr
}
\keyword{ package }
\note{
All candlestick pattern detection functions \code{(CSP*)} preserve the xts time series' attributes and add one attribute \code{bars}, that represents the number of bars the formation consists of.
}
\seealso{

}
\examples{

}
