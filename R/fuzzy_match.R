#-------------------------------------------------------------------------#
#------------------------------ FUZZY MATCH ------------------------------#
#-------------------------------------------------------------------------#

# FUZZYMATCHPAIRS
# this function finds the best matching pair for each value in vector x in vector y
# it outputs a two-column dataframe

# author: alice stuart | date modified: 2020-0x-xx
# compiled in R version 3.6.2 (2019-12-12) Dark and Stormy Night running x86_64-apple-darwin15.6.0


### FUNCTION -------------------------------------------------------------
#' @export
fuzzymatchpairs <- function(x,y){
  # create a matrix with the Standard Levenshtein distance between the two vectors
  dist.name <- adist(tm::removePunctuation(x),
                     tm::removePunctuation(y),
                     partial = TRUE,
                     ignore.case = TRUE,
                     costs = c(1, 0.5, 1))

  # find pairs with minimum distance
  match.x.y <- tidyr::tibble(x.i = 1:nrow(dist.name),
                             x.name = x[x.i],
                             y.i = apply(dist.name, 1, which.min),
                             y.name = y[y.i],
                             distance = apply(dist.name, 1, min))

  return(match.x.y)
}

# FUZZYMATCHPOSITION
#' @export
fuzzymatchposition <- function(x,y){
  # create a matrix with the Standard Levenshtein distance between the name fields of both sources
  dist.name<-adist(x,y, partial = FALSE, ignore.case = TRUE)

  # find pairs with minimum distance
  min.name<-apply(dist.name, 1, min)

  result <- position(min.name,dist.name)

  return(result)
}
