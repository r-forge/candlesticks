\name{CSPTasukiGap}
\alias{CSPTasukiGap}
\alias{TasukiGap}
\alias{UpsideTasukiGap}
\alias{DownsideTasukiGap}
\title{Upside/Downside Tasuki Gap Candlestick Pattern}
\description{Look for Upside/Downside Tasuki Gap Pattern in a OHLC price series}
\usage{CSPTasukiGap(TS)}
\arguments{
  \item{TS}{xts Time Series containing OHLC prices}
}
\details{
Number of candle lines: \bold{3}\cr\cr
\emph{Upside Tasuki Gap:}\cr
The market is in uptrend. The first candle of the formation is a white candle, followed by another white candle that has gapped above the high of the first candle. The third candle is a black candle that opens within the body of the second candle and closes within the gap between the first and second candle.\cr\cr
\emph{Downside Tasuki Gap:}\cr
The market is in downtrend. The first candle of the formation is a black candle, followed by another black candle that has gapped below the low of the first candle. The third candle is a white candle that opens within the body of the second candle and closes within the gap between the first and second candle.\cr\cr
}
\value{
  A xts object containing the columns:
  \item{UpsideTasukiGap}{TRUE if Upside Tasuki Gap pattern detected}
  \item{DownsideTasukiGap}{TRUE if Downside Tasuki Gap pattern detected}
}
\author{Andreas Voellenklee}
\references{
The following site(s) were used to code/document this indicator:\cr
  \url{http://www.investopedia.com/terms/u/upside-tasuki-gap.asp#axzz1jtuDsM6i}\cr
  \url{http://www.investopedia.com/terms/d/downside-tasuki-gap.asp#axzz1jtuDsM6i}\cr
  \url{http://thepatternsite.com/UpsideTasukiGap.html}\cr
  \url{http://thepatternsite.com/DownsideTasukiGap.html}\cr
}
\note{The function filters patterns that look like tasuki gap patterns, without considering the current trend direction. If only upside tasuki gaps in uptrends should be filtered, a external trend detection function must be used. See examples.}
\seealso{
\code{\link{CSPGap}}
}
\examples{
\dontrun{
  getSymbols("SSRI", adjust=TRUE)
  CSPTasukiGap(SSRI)

  # filter upside tasuki gaps in uptrends
  CSPTasukiGap(SSRI)[,"UpsideTasukiGap"] & TrendDetectionChannel(lag(SSRI,k=5))[,"UpTrend"]
}
}
\keyword{}
