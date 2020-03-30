#-------------------------------------------------------------------------#
#---------------------------- ANALYSE REFS  ------------------------------#
#-------------------------------------------------------------------------#

# ANALYSE REFERENCES
# this function takes urls for species homepages and scrapes the alternate latin names for each spp
# it returns a list with one element for each spp, containing a dataframe of all EOL latin names
# requires rvest

# author: alice stuart | date modified: 2020-03-14
# compiled in R version 3.6.3 (2020-02-29) -- "Holding the Windsock" running x86_64-apple-darwin15.6.0

#' @export
cleanauthorslist <- function(reference_data){
  split_authors <- stringr::str_split(reference_data$author,' and ')
  # clean up author case
  split_authors_df <- data.frame(referenceId = rep(x = reference_data$reference_id,times=sapply(split_authors,length)),
                                 author = unlist(split_authors),
                                 surname = NA,
                                 otherNames = NA)
  split_authors_df[,3] <- sapply(split_authors_df$author, function(x) unlist(stringr::str_split(x,pattern = ', '))[1])
  split_authors_df[,4] <- sapply(split_authors_df$author, function(x) unlist(stringr::str_split(x,pattern = ', '))[2])
  split_authors_df[,3] <- stringr:::str_to_title(split_authors_df[,3])
  split_authors_df[,4] <- tm::removePunctuation(split_authors_df[,4])
  tempOtherNames <- gsub(pattern = '[-]| ',replacement = '',x = split_authors_df$otherNames)
  tempOtherNames <- sapply(tempOtherNames, function(x) gsub(pattern = '[a-z]', replacement = '',x))
  split_authors_df[,4] <- tempOtherNames
  split_authors_df$cleanedName <- paste(split_authors_df$surname,split_authors_df$otherNames,sep = ', ')
}

