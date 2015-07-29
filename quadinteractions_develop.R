# Quadratic interactions
# TODO generalise for higher order interactions?

quadInteraction <- function(df){
  # order interaction hard coded for now to 2
  # input checking - make sure no NA etc in the input??
  
  # locate the numeric columns
  # then select them. save into dfNum 
  numericCols <- vapply(df, is.numeric, logical(1)) 
  dfNum <- df[, numericCols]
  
  # number of columns for output is ncol initial
  # plus n choose 2 + n where n is number of numeric
  nCols <- ncol(df) + choose(ncol(dfNum), 2) + ncol(dfNum)
  
  # initilise output df full of NA's with correct number columns
  out <- data.frame(matrix(NA, nrow = nrow(df), ncol = nCols))
  
  # fill in first part with initial df
  out[1:ncol(df)] <- df
  names(out)[1:ncol(df)] = names(df)
  
  # calculate interaction terms
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

data(mtcars)
interaction(mtcars)
# obviously have to be careful as am, gear are factors really

test <- data.frame(x = c(1, 1, 2, 8.5),
                   y = c(2, 5.2, 1.6, 2.1),
                   z = c(T, T, F, T),
                   fac = c('hat', 'cat', 'rat', 'mat'),
                   blah = c(2, 3, 5.5, 6)
                   )

test
quadInteraction(test)