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
