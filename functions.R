## functions

library(ggplot2)
library(ggsci)

makeList <- function(x){
  rlt <- list()
  for(i in 1:length(x)){
    rlt[[i]] <- i
  }
  names(rlt) <- x
  rlt
}

formatRatio <- function(x){
  if(abs(x)>=0.1){
    format(round(x, 2), nsmall = 2)
  } else if (abs(x)>=0.01) {
    format(round(x, 3), nsmall = 3)
  } else if (abs(x)>=0.001) {
    format(round(x, 4), nsmall = 4)
  } else {
    sprintf("%.2e", x)
  }
}

