# function to take in a vector of search strings, vector of replacement strings
# option to use regexp or direct subsetting (which may be faster,
# although not as flexible)

# TODO:
# implement foreach to do in parallel?

replace <- function(x, curr, replace, regex = FALSE, ...) {
  stopifnot(length(curr) == length(replace))
  stopifnot(is.character(curr) && is.character(replace))
  
  for(i in 1:length(curr)){ 
    if(regex){
      x <- gsub(curr[i], replace[i], x, ...)
    } else {
      x[x == curr[i] ] <- replace[i]
    }
  }
  x
}

test <- data.frame(x = c('b', 'b', 'h', 'k', 'k', '1', '+'),
                   y = c(1, 2, 3, 4, 5, 6, 7))
test$x <- as.character(test$x)

abbrev <- c('b', 'k', '1', '+')
desired <- c('1e9', '1e3', '0', '0')

test$x
replace(test$x, abbrev, desired)



test2 <- data.frame(x = c('b', 'B', 'b', 'B', 'b', 'b', 'b'),
                    y = c(1, 2, 3, 4, 5, 6, 7))

test2$x <- as.character(test2$x)
abbrev2 <- c('[bB]')
desired2 <- c('1e9')

test2$x
replace(test2$x, abbrev2, desired2, regex = T)