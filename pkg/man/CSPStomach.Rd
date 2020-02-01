\name{CSPStomach}
\alias{CSPStomach}
\alias{Stomach}
\alias{AboveTheStomach}
\alias{BelowTheStomach}
\title{Above/Below The Stomach Candlestick Pattern}
\description{Look for Above/Below The Stomach Pattern in a Open/Close Time Series}
\usage{CSPStomach(TS)}
\arguments{
  \item{TS}{xts Time Series containing Open and Close Prices}
}
\details{
Number of candle lines: \bold{2}\cr\cr
\emph{Above The Stomach:}\cr
Prior trend: \bold{down}\cr
A black candle followed by a white candle. The opening price of the second day must be at or above the mid point of the first candle's body.\cr\cr
\emph{Below The Stomach:}\cr
Prior trend: \bold{up}\cr
A white candle followed by a black candle. The opening price of the second day must be at or below the mid point of the first candle's body.\cr
}
\value{
  A xts object containing the columns:
  \item{AboveTheStomach}{TRUE if Above The Stomach pattern detected}
  \item{BelowTheStomach}{TRUE if Below The Stomach pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{http://www.thepatternsite.com/AboveStomach.html}\cr
  \url{http://www.thepatternsite.com/BelowStomach.html}
}
\note{The function filters patterns that look like above/below the Stomach, without considering the current trend direction. If only above the Stomach pattern in downtrends should be filtered, a external trend detection function must be used. See examples.}
\examples{
\dontrun{
  getSymbols('YHOO',adjust=TRUE)
  CSPStomach(YHOO)
  
  # filter above the Stomach pattern in downtrends
  CSPStomach(YHOO)[,"AboveTheStomach"] & TrendDetectionChannel(lag(YHOO,k=2))[,"DownTrend"]
}
}
