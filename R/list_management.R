#-------------------------------------------------------------------------#
#---------------------------- LIST MANAGEMENT ----------------------------#
#-------------------------------------------------------------------------#

# EXPORTLIST
# this function exports the elements of a list as individual .csv files
# a csv for each list element

# author: alice stuart | date modified: 2020-02-28
# compiled in R version 3.6.2 (2019-12-12) Dark and Stormy Night running x86_64-apple-darwin15.6.0

exportlist <- function(list, directory, name){
  current_wd <- getwd()
  setwd(directory)
  for(i in 1:length(list)){
    directory <- as.character(directory)
    write.csv(x = list[i], file = paste(name,'_',names(list)[i],'.csv',sep='',collapse=''))
  }
  setwd(current_wd)
}

# IMPORTCSVLIST
# this function imports all .csv files with a common string in their name into a list
# list with each csv as an element

# author: alice stuart | date modified: 2020-02-28
# compiled in R version 3.6.2 (2019-12-12) Dark and Stormy Night running x86_64-apple-darwin15.6.0

importcsvlist <- function(directory, pattern){
  current_wd <- getwd()
  setwd(directory)
  files <- list.files(pattern = paste(pattern,".*","csv$",sep='',collapse=''))
  result <- lapply(files, readr::read_csv)
  names(result) <- gsub(x = files, pattern = '.csv',replacement = '')
  names(result) <- gsub(x = names(result), pattern = pattern, replacement = '')
  for(i in 1:length(result)){
    colnames(result[[i]]) <- gsub(x = colnames(result[[i]]), pattern = paste(names(result)[i],'[.]',sep='',collapse = ''), replacement = '')
    result[[i]] <- result[[i]][,-1]
  }
  setwd(current_wd)
  return(result)
}

