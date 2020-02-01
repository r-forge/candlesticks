\name{addPriceInfo}
\alias{addPriceInfo}
\title{Add OHLC Information for detected candlestick patterns}
\description{
This function returns not only the occurences of candlestick patterns, but also the OHLC price information of the pattern's formation. The OHLC price information is only set on dates when the pattern is detected, and 0 otherwise.
}
\usage{addPriceInfo(TS, CSP)}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
  \item{CSP}{xts Time Series of detected candlestick patterns, based on TS}
}
\details{
  The xtsAttribute \code{bars} of \code{CSP} is used to calculate the blended candlestick of the formation.
}
\value{
A xts object containing the columns:
  \item{-}{all columns of CSP}
  \item{Formation.Open}{Opening price of the detected candlestick formation}
  \item{Formation.High}{Highest high of the detected candlestick formation}
  \item{Formation.Low}{Lowest low of the detected candlestick formation}
  \item{Foramtion.Close}{Close price of the detected candlestick formation}
}
\author{Andreas Voellenklee}
\seealso{
  \code{\link{CSPNBlended}}
}
\examples{
\dontrun{
getSymbols("YHOO", adjust=TRUE)

# return detected Engulfing Pattern
CSPEngulfing(YHOO)

# returns detected Engulfing Pattern including formation's OHLC
addPriceInfo(YHOO, CSPEngulfing(YHOO))
}
}
