nextCandlePosition <- function(TS) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  NextOp <- as.xts(Next(Op(TS)))
  NextCl <- as.xts(Next(Cl(TS)))
  HigherOpen <- NextOp > Op(TS)
  LowerOpen <- NextOp < Op(TS)
  HigherClose <- NextCl > Cl(TS)
  LowerClose <- NextCl < Cl(TS)
  NextWhite <- NextOp < NextCl
  NextBlack <- NextOp > NextCl
  result <- reclass(cbind(HigherOpen, LowerOpen, HigherClose, LowerClose, NextWhite, NextBlack), TS)
  colnames(result) <- c("HigherOpen", "LowerOpen","HigherClose","LowerClose", "White", "Black")
  return(result)
}