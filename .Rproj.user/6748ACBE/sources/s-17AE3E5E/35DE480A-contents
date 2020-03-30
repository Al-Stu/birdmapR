#-------------------------------------------------------------------------#
#--------------------------- COMBINE AND SORT ----------------------------#
#-------------------------------------------------------------------------#

# COMBINEANDSORT
# combines two dataframes of names (mist have the same column names) and removes duplicate names, where two different species have the same name, it will keep the one for which the name is main

# author: alice stuart | date modified: 2020-03-12
# compiled in R version 3.6.3 (2020-02-29) -- "Holding the Windsock" running x86_64-apple-darwin15.6.0
#' @export
combineandsort <- function(names1,names2){
  # remove blank sci names
  combined <- rbind(names1,names2)
  combined <- combined[!(is.na(combined$name) | combined$name=="" | is.null(combined$name)), ]

  # remove duplicate sci names within a spp.
  unique_rows <- !duplicated(combined[c('internalTaxonId',"name")])
  combined <- combined[unique_rows,]
  combined <- combined[order(combined$internalTaxonId),]
  rownames(combined) <- c(1:nrow(combined))
  if(any(duplicated(combined["name"]))){
  # remove duplicate sci name if it's not the main name for that spp.
    duplicates <- combined[duplicated(combined["name"]) | duplicated(combined["name"], fromLast = TRUE),]
    to_remove <- duplicates[duplicates$main!='true',]
    combined <- combined[-as.numeric(rownames(to_remove)),]
  }
  rownames(combined) <- c(1:nrow(combined))
  if(nrow(combined)!=length(unique(combined$name))){
    warning('it has not been possible to remove all duplicates, please check manually')
  }
  return(combined)
}
