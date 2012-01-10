CandleLength <- function(TS) {
  if (!is.HL(TS)) {
    stop("Price series must contain High and Low.")
  }
  RCL <- 2*(Hi(TS)-Lo(TS)) / (Hi(TS)+Lo(TS))
  ACL <- Hi(TS)-Lo(TS)
  result <- cbind(RCL, ACL)
  colnames(result) <- c("relCandleLength", "absCandleLength")
  return(result)
}

CandleBodyLength <- function(TS) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  RCBL <- 2*(abs(Op(TS)-Cl(TS))) / (Op(TS)+Cl(TS))
  ACBL <- abs(Op(TS)-Cl(TS))
  result <- cbind(RCBL, ACBL)
  colnames(result) <- c("relCandleBodyLength", "absCandleBodyLength")
  return(result)
}