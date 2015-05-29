# this is my ggplot2 version of lattice denistyplot()
# often handy to have all predictors in panels for machine
# learning problems!
# Aim: to automatically cycle through predictors, and plot
# ggplot density plots to examine distributions 

library(ggplot2)
library(mlbench)

# load and summary of data
data(Glass)
str(Glass)

# make a list of ggplot2 objects
colNames <- names(Glass)[1:9]
j <- 1
plotList <- list()
for(i in colNames){
  plt <- ggplot(Glass, aes_string(x=i)) + geom_density() + geom_rug()
  assign(paste("plot", j, sep = ""), plt)   
  j <- j + 1
  plotList[[i]] <- plt
}

# could cycle through plots one by one...
for(i in colNames){
  plt <- ggplot(Glass, aes_string(x=i)) + geom_density()  + geom_rug()
  print(plt)
  Sys.sleep(2)
}

# but a multiplot would be far more handy! 
# this modifies http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# reads in a list of plots (which may be tedious to do one by one) and plots!

multiplotList(plotList[1:9],cols=3)

multiplotList <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <-  c(..., plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
