#-------------------------------------------------------------------------#
#--------------------------------- FOCUS ---------------------------------#
#-------------------------------------------------------------------------#

# finds search string with highest number of instances for each paper

# author: alice stuart | date modified: 2020-02-28
# compiled in R version 3.6.2 (2019-12-12) Dark and Stormy Night running x86_64-apple-darwin15.6.0

focus <- function(matrix){
  position_max <- vector(mode = 'numeric', length = ncol(matrix))
  result <- vector(mode = 'numeric', length = ncol(matrix))
  for(i in 1:ncol(matrix)){
    if(all(matrix[,i] == 0)){
      result[i] <- NA
    }else{
      position_max[i] <- which.max(matrix[,i])
      result[i] <- rownames(matrix)[position_max[i]]
    }
  }
  return(result)
}
