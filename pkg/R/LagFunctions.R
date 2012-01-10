# Function is.OC and is.HL are not included in quantmod
is.OC <- function (x)
{
  if (has.Op(x) & has.Cl(x)) { 
     TRUE
  }
  else FALSE
}

is.HL <- function (x)
{
  if (has.Hi(x) & has.Lo(x)) { 
     TRUE
  }
  else FALSE
}

LagOHLC <- function(TS, k=1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  result <- cbind(Lag(Op(TS),k), Lag(Hi(TS),k), Lag(Lo(TS),k), Lag(Cl(TS),k))
  colnames(result) <- c(
    paste(colnames(Op(TS)),k,sep='.Lag.'),
    paste(colnames(Hi(TS)),k,sep='.Lag.'),
    paste(colnames(Lo(TS)),k,sep='.Lag.'),
    paste(colnames(Cl(TS)),k,sep='.Lag.'))
  return(result)
}

LagOC <- function(TS, k=1) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  result <- cbind(Lag(Op(TS),k), Lag(Cl(TS),k))
  colnames(result) <- c(
    paste(colnames(Op(TS)),k,sep='.Lag.'),
    paste(colnames(Cl(TS)),k,sep='.Lag.'))
  return(result)
}
