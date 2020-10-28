#-------------------------------------------------------------------------#
#-------------------------------- WITHIN ---------------------------------#
#-------------------------------------------------------------------------#

# WITHIN
# this function tests if x (two numbers representing the start and end of a string) is within any of y (dataframe with two columns representing the start and end of a strings)
# it outputs true if x is contained within y and false if not

# author: alice stuart | date modified: 2020-04-03
# compiled in R version 3.6.2 (2019-12-12) Dark and Stormy Night running x86_64-apple-darwin15.6.0
#' @export
within <- function(x,y){
  if(length(y) == 0){
    result <- NULL
  }else if(length(y) == 2){
    result <- y[1] <= x[1] & y[2] >= x[2]
  }else{
    result <- y[,1] <= x[1] & y[,2] >= x[2]
  }
  return(result)
}

# WITHINLIST
# this function applies the within function to a list
# it outputs list of true/false vectors

# author: alice stuart | date modified: 2020-0x-xx
# compiled in R version 3.6.2 (2019-12-12) Dark and Stormy Night running x86_64-apple-darwin15.6.0
#' @export
withinlist <- function(list, x){
  result <- list()
  for (i in 1:length(list)) {
    result[[i]] <- within(x = x, y = list[[i]])
  }
  return(result)
}
