\name{CSPNBlended}
\alias{CSPNBlended}
\alias{BlendedCandles}
\title{Calculate Open/High/Low/Close of N bars}
\description{This function calculates OHLC prices of a combined candle of N bars.}
\usage{CSPNBlended(TS, N)}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
  \item{N}{number of bars to combine into one bar}
}
\value{
A xts object containing the columns:
  \item{<N>.Blended.Open}{Opening price of the \code{N}-th elapsed candle}
  \item{<N>.Blended.High}{Highest high of the past \code{N} candles}
  \item{<N>.Blended.Low}{Lowest low of the past \code{N} candles}
  \item{<N>.Blended.Close}{Close price of the current candle}
}
\author{Andreas Voellenklee}
\note{This function is used by \code{\link{addPriceInfo}} to add price information of detected candlestick patterns}
\seealso{
\code{\link{addPriceInfo}}
}
\examples{
\dontrun{
getSymbols("YHOO", adjust=TRUE)
CSPNBlended(YHOO, N=3)  # combine 3 candles into one
}
}