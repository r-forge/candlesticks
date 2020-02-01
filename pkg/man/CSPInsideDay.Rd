\name{CSPInside}
\alias{CSPInsideDay}
\alias{CSPOutsideDay}
\title{Inside / Outside Day}
\description{Look for inside days and outside days in a OHLC price series}
\usage{
  CSPInsideDay(TS)
  CSPOutsideDay(TS)
}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
}
\details{
  Number of candle lines: \bold{2}\cr\cr
  \emph{Inside Day:}\cr
  In an inside day, the bar remains within the previous day's bar high and low.\cr\cr
  \emph{Outside Day:}\cr
  In an outside day, the bar's high is higher than the previous day's bar high and the low lower than the previous day's low.
}
\value{
  A xts object containing the column:
  \item{InsideDay}{TRUE if current candle is a inside day}
  \item{OutsideDay}{TRUE if current candle is a outside day}  
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{http://www.investopedia.com/terms/i/inside_day.asp#axzz1jWelu1cm}\cr
  \url{http://www.investopedia.com/terms/o/outside-days.asp#axzz1jWelu1cm}
}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPInsideDay(YHOO)
  CSPOutsideDay(YHOO)
}
}
