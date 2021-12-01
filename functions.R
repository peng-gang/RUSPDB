## functions

read_RData <- function(){
  df <- load('/Users/oliviazhang/Desktop/500KClean.RData') 

  # Convert to data table
  df <- as.data.table(df)
  
  df
}
