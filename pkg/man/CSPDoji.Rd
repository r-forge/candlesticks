\name{CSPDoji}
\alias{CSPDoji}
\alias{Doji}
\title{Doji Candlestick Pattern}
\description{Look for Doji Pattern in a OHLC price series}
\usage{CSPDoji(TS, DojiBLRatio=.1)}
\arguments{
  \item{TS}{xts Time Series containing Open and Close Prices}
  \item{DojiBLRatio}{maximum body to lenght ratio, default = .1}
}
\details{
Number of candle lines: \bold{1}\cr
A Doji is formed when the open and the close are the same or very close. By default settings, a doji is identified when the body of the candle is smaller or equal 1/10 of its full lenght.
}
\value{
  A xts object containing the column:
  \item{Doji}{TRUE if Doji pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this candlestick pattern:\cr
  \url{http://www.onlinetradingconcepts.com/TechnicalAnalysis/Candlesticks/Doji.html}
}
\note{}
\seealso{}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPDoji(YHOO)
  CSPDoji(YHOO, DojiBLRatio=0) # filter for doji patterns 
                               # that have exactly equal 
                               # open and close prices
}
}
\keyword{}
