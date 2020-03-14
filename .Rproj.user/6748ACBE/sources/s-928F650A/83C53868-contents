#-------------------------------------------------------------------------#
#-------------------------- CREATE SEARCH TERMS  --------------------------#
#-------------------------------------------------------------------------#

# CREATESEARCHTERMS
# create search terms to be used in regex, WoS and Scopus

# author: alice stuart | date modified: 2020-03-13
# compiled in R version 3.6.3 (2020-02-29) -- "Holding the Windsock" running x86_64-apple-darwin15.6.0

# NEED TO:
# only need one case and full length sci names in external search terms
# put scopus search into a different list which splits terms into strings of the correct length to be put into scopus in diff rows

createsearchterms <- function(all_common_names,all_scientific_names){
  searchterms <-data.frame(internalTaxonId = unique(all_scientific_names$internalTaxonId),
                           regex_sci = NA,
                           regex_common = NA,
                           WoS = NA)
  for(i in 1:nrow(searchterms)){
    temp_common <- as.character(all_common_names$name[all_common_names$internalTaxonId==searchterms$internalTaxonId[i]])
    temp_common <- stringr::str_trim(temp_common)
    temp_sci <- as.character(all_scientific_names$name[all_scientific_names$internalTaxonId==searchterms$internalTaxonId[i]])
    searchterms$WoS[i] <- proximitysearchterm(search_terms = c(temp_common,temp_sci),WOS_or_SCOPUS = "WOS")
    temp_common <- unique(c(temp_common,tolower(temp_common)))
    temp_common <- stringr::str_trim(temp_common)
    abbrev_sci <- as.character(abbrevsciname(sci_names = temp_sci,
                  ids = rep_len(searchterms$internalTaxonId[i],length(temp_sci)))$name)
    temp_sci <- c(temp_sci,abbrev_sci)
    searchterms$regex_sci[i] <- paste(temp_sci,sep='|',collapse='|')
    searchterms$regex_common[i] <- paste(temp_common,sep='|',collapse='|')
  }
  return(searchterms)
}

# SCOPUScommon <- proximitysearchterm(search_terms = temp_common,WOS_or_SCOPUS = "SCOPUS")
# SCOPUSsci <- proximitysearchterm(search_terms = temp_sci,WOS_or_SCOPUS = "SCOPUS")
# searchterms$SCOPUS[i] <- paste('(',SCOPUScommon,')',' AND ','(',SCOPUSsci,')',sep='',collapse='')

# ABBREVSCINAME
# input scinames and associated ids, outputs a dataframe with abbrev (T tetrax tetrax) and double abbrev (T t tetrax) scinames associated with ID

abbrevsciname <- function(sci_names,ids){
  scinames <- stringr::str_split(sci_names,pattern = ' ')
  first_letter <- substr(x = sapply(scinames, function(x) x[1]),start = 1,stop = 1)
  second_word <- sapply(scinames, function(x) x[2])
  third_word <- sapply(scinames, function(x) x[3])
  end <- gsub(pattern = ' NA$',replacement = '',x = paste(second_word,third_word,sep = ' '))
  abbrev_sciname <- paste(first_letter,end,sep= ' ')
  result <- cbind.data.frame(ids,abbrev_sciname)
  second_letter <- substr(x = second_word,start = 1,stop = 1)
  double_abbrev_sciname <- paste(first_letter,second_letter,third_word,sep= ' ')
  double_abbrev_sciname <- cbind.data.frame(ids,double_abbrev_sciname)
  double_abbrev_sciname <- double_abbrev_sciname[!grepl(pattern = ' NA$',
                           x = double_abbrev_sciname$double_abbrev_sciname),]
  colnames(result) <- c('internalTaxonId','name')
  colnames(double_abbrev_sciname) <- c('internalTaxonId','name')
  result <- rbind(result,double_abbrev_sciname)
  result <- result[order(result$internalTaxonId),]
  rownames(result) <- c(1:nrow(result))
  return(result)
}

# PROXIMITYSEARCHTERM
# uses proximity operators to create a WOS or SCOPUS searchterm
proximitysearchterm <- function(search_terms, WOS_or_SCOPUS){
  split_search <- stringr::str_split(search_terms,pattern = ' ')
  if(WOS_or_SCOPUS =='WOS'){
    proximity_search <- sapply(X = split_search,FUN = function (x) paste(x,collapse = ' NEAR/0 '))
  } else if (WOS_or_SCOPUS =='SCOPUS'){
    proximity_search <- sapply(X = split_search,FUN = function (x) paste(x,collapse = ' pre/0 '))
  } else {
    stop("database incorrectly set, please set WOS_or_SCOPUS to 'WOS' for Web of Science or 'SCOPUS' for SCOPUS ")
  }
  proximity_search <- paste(proximity_search,collapse = ' OR ')
  return(proximity_search)
}
