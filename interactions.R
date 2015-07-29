quadInteraction <- function(df){
  numericCols <- vapply(df, is.numeric, logical(1)) 
  dfNum <- df[, numericCols]
  nCols <- ncol(df) + choose(ncol(dfNum), 2) + ncol(dfNum)
  
  out <- data.frame(matrix(NA, nrow = nrow(df), ncol = nCols))
  out[1:ncol(df)] <- df
  names(out)[1:ncol(df)] = names(df)
  
  start <- 1
  dfPosition <- ncol(df) + 1
  for( i in 1:ncol(dfNum) ){
    for( j in start:ncol(dfNum) ){
      out[dfPosition] <- dfNum[i] * dfNum[j]
      names(out)[dfPosition] <- paste(names(dfNum[i]),
                                      names(dfNum[j]),
                                      sep = ' * ')
      dfPosition <- dfPosition + 1
    }
    start <- start + 1 
  }
  
  out
}
