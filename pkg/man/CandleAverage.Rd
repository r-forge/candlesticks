\name{CandleAverage}
\alias{OHLC_Average}
\alias{HLC_Average}
\alias{HL_Average}
\title{Mean value of OHLC prices}
\description{Calculates the average value of different price levels within a price bar.}
\usage{
  OHLC_Average(TS)
  HLC_Average(TS)
  HL_Average(TS)
}
\arguments{
  \item{TS}{xts Time Series containing Open and Close Prices}
}
\details{
The calculation of the average of the high, low, and close, is as follows:\cr
OHLC Average = (Open + High + Low + Close) / 4\cr
HLC Average = (High + Low + Close) / 3\cr
HL Average = (High + Low) / 2\cr
}
\value{
  A xts object containing the corresponding column:
  \item{OHLC_Average}{}
  \item{HLC_Average}{}
  \item{HL_Average}{}
}
\examples{
\dontrun{
# calculate average prices for a daily OHLC price series
getSymbols('YHOO', adjust=TRUE)
head(cbind(OHLC_Average(YHOO), 
      HLC_Average(YHOO), 
      HL_Average(YHOO)))
}
}
\author{Andreas Voellenklee}