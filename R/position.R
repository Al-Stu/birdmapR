#-------------------------------------------------------------------------#
#------------------------------- POSITION --------------------------------#
#-------------------------------------------------------------------------#

# this function finds the postition of x in a vector y
# it outputs a vector with the position(s) of where y == x

# author: alice stuart | date modified: 2020-02-28
# compiled in R version 3.6.2 (2019-12-12) Dark and Stormy Night running x86_64-apple-darwin15.6.0

position <- function(x,y){
  result <- grep(pattern = 'TRUE', x = y==x)
  return(result)
}
