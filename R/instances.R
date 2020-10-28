#-------------------------------------------------------------------------#
#------------------------------- INSTANCES -------------------------------#
#-------------------------------------------------------------------------#

# INSTANCES
# this function searches a vector of string of terms seperated by | in a vector of paper texts
# it returns a matrix with number of instances of each search term in each
# requires stringr to work


# author: alice stuart | date modified: 2020-02-28
# compiled in R version 3.6.2 (2019-12-12) Dark and Stormy Night running x86_64-apple-darwin15.6.0

instances <- function(x, search_string, x_id, search_id){
  result <- matrix(nrow = length(search_string), ncol = length(x))
  for(i in 1:length(search_string)){
    locations <- stringr::str_locate_all(string = x, pattern = search_string[i]) # create list with start and end of each instance of search string in each paper text
    no_uses <- as.numeric(lapply(locations, length))/2 # list with number of instances of search string in each paper text, as locations is a list, length returns number of items in the list (i.e. nrow x ncol), two columns so divide by two to get nrow (should probably just use nrow function)
    result[i,] <- unlist(no_uses) # put number of instances into vector
  }
  rownames(result) <- search_id
  colnames(result) <- x_id
  return(result)
}

#' INSTANCESNOOVERLAP
#'
#' as instances but removes all cases where one search term is returned inside another search term
#' e.g. if 'owl' and 'scops owl' are both search terms, any time scops owl is written, it will not count as an instance of owl
#' it returns a matrix with number of instances of each search term in each requires stringr to work
#'
#' @param x vector of texts to be searched within
#' @param search_string vector of strings to search
#' @param x_id vector of ids corresponding to texts in \code{x}
#' @param search_id vector of ids corresponding to search terms in \code{search_string}
#'
#' @return matrix with number of instances of each search term in each text
#'
#' @import stringr
#'
#' @export

instancesNoOverlap <- function(x, search_string, x_id, search_id){
  result <- matrix(nrow = length(search_string), ncol = length(x))
  for(p in 1:length(x)){
    locations <- stringr::str_locate_all(string = x[p], pattern = search_string) # create list with start and end of each instance of search string in each paper text
    locations_2 <- locations
    for(j in 1: length(locations)){
      if(length(locations[[j]])==0){
        locations_2[[j]] <- locations[[j]]
      }else{
        if(length(locations[[j]])==2){
          uses <- withinlist(locations,locations[[j]])
          if(sum(sapply(uses,sum))>1){
            locations_2[[j]] <- data.frame(start=double(),end=double())
          }
        }else{
          if(length(locations[[j]])>2){
            to_delete <- c()
            for(l in nrow(locations[[j]]):1){
              uses <- withinlist(locations,locations[[j]][l,])
              if(sum(sapply(uses,sum))>1){
                to_delete <- c(to_delete,l)
              }
            }
            if(length(to_delete)!=0){
              locations_2[[j]] <- locations[[j]][-to_delete,]
            }
          }
        }
      }
    }
    no_uses <- as.numeric(lapply(locations_2, length))/2 # list with number of instances of search string in each paper text, as locations is a list, length returns number of items in the list (i.e. nrow x ncol), two columns so divide by two to get nrow (should probably just use nrow function)
    result[,p] <- unlist(no_uses) # put number of instances into vector
  }
  rownames(result) <- search_id
  colnames(result) <- x_id
  return(result)
}
