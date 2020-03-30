#-------------------------------------------------------------------------#
#---------------------------- IOC BIRD LIST  -----------------------------#
#-------------------------------------------------------------------------#

# IOC.CURRENTVER
# this function finds the most recent version of the IOC bird list
# it returns a version number
# requires rvest

# author: alice stuart and matt lewis | date modified: 2020-03-12
# compiled in R version 3.6.3 (2020-02-29) -- "Holding the Windsock" running x86_64-apple-darwin15.6.0

## NOT WORKING, PROBLEM W/ NODES IN TEXTBOXFROMURL
#' @export
IOC.currentver <- function(){
  text <- textboxfromurl('https://www.worldbirdnames.org/ioc-lists/master-list-2/','//*[@id="header"]')[2]
  text <- gsub(pattern = '^.*version',x = text, replacement = 'version')
  text <- gsub(pattern = '\\sHome$',x = text, replacement = '')
  text <- unlist(strsplit(text, split = '\\s'))
  return(text)
}

# IOC.DOWNLOAD
#' @export
IOC.download <- function(version){
  res <- httr::GET("https://www.worldbirdnames.org/ioc-lists/master-list-2/")
  doc <- xml2::read_html(httr::content(res, as='text'))
  nodes <- rvest::html_nodes(doc, ".post-bodycopy")
  node_text <- unlist(as.character(nodes[[1]]))
  links <- as.character(stringr::str_extract_all(string = node_text, pattern = 'href=["].*["]'))
  links <- strsplit(links,',')
  links <- links[[1]][grepl(pattern = 'vs_other', x = links[[1]])]
  links <- gsub(pattern = '^.*\\\"https:', replacement = 'https:', x = links)
  links <- gsub(pattern = 'xlsx.*$', replacement = 'xlsx', x = links)
  correct_link <- links[grepl(pattern = version,x=links)]
  httr::GET(correct_link, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
  df <- readxl::read_excel(tf, 1L)
  return(df)
}

# IOC.FORMAT
# formats IOC data into a tibble with species ID and scientific name according to each taxonomy
#' @export
IOC.format <- function(IOCsheet){
  columns <- colnames(IOCsheet)
  av_string_length <- averagestringlength(IOCsheet)
  keep_cols <- (grepl(pattern = 'rank|notes|group|family|volume|red list category', x = columns, ignore.case = TRUE) == FALSE
                & av_string_length >=3)
  taxonomies <- IOCsheet[,keep_cols]
  columns <- colnames(taxonomies)
  IUCN_column <- as.numeric(grep(pattern = 'Handbook of the Birds of the World and BirdLife International digital checklist of the birds of the world',
                                 x = columns, ignore.case = TRUE))
  taxonomies <- tidyr::fill(taxonomies,IUCN_column)
  taxonomies <- taxonomies[,c(1,IUCN_column,c(2:(IUCN_column-1)),c((IUCN_column+1):ncol(taxonomies)))]
  colnames(taxonomies)[2] <- c(columns[IUCN_column])
  taxonomies <- tidyr::as_tibble(taxonomies)
  return(taxonomies)
}

# averagestringlength
# finds average string length of each column of a dataframe
averagestringlength <- function(df){
  av_string_length <- c()
  for(i in 1:ncol(df)){
    temp_names <- na.omit(df[,i])
    temp_length <- NULL
    temp_length <- apply(temp_names,1,nchar)
    av_string_length[i] <- mean(temp_length)
  }
  return(av_string_length)
}

# IOC.ALTNAMES
#' @export
IOC.altnames <- function(formattedIOC,cleaned_redlist_data){
  # select only target spp.
  temp_colname <- colnames(formattedIOC)[2]
  colnames(formattedIOC)[2] <- 'scientificName'
  altnames <- merge(cleaned_redlist_data[['species_data']][,c(1:2)],formattedIOC[,-1])
  altnames <- altnames[,c(2,1,c(3:ncol(altnames)))]
  colnames(altnames)[2] <- temp_colname
  altnames_gathered <- tidyr::gather(altnames, database, name, -internalTaxonId)
  # combine names into one column and add to species_data
  altnames_gathered$main <- c(rep_len('true',nrow(altnames_gathered[altnames_gathered$database==temp_colname,])),
                              rep_len('false',nrow(altnames_gathered[altnames_gathered$database!=temp_colname,])))
  all_scientific_names <- altnames_gathered[,c('internalTaxonId','name','main')]
  all_scientific_names$name <- as.character(all_scientific_names$name)
  # remove duplicated names
  all_scientific_names <- unique(tidyr::as_tibble(all_scientific_names)[!is.na(all_scientific_names$name),])
  duplicated_rows <- duplicated(all_scientific_names[c('internalTaxonId','name')])
  dup_row_check <- all_scientific_names[duplicated_rows,]
  if(all(dup_row_check$main == 'false')){
    all_scientific_names <- all_scientific_names[!duplicated_rows,]
  } else {
    warning('something went wrong removing duplicates')
  }
  all_scientific_names <- all_scientific_names[order(all_scientific_names$internalTaxonId),]
  all_scientific_names$source <- rep_len('IOC', nrow(all_scientific_names))
  return(all_scientific_names)
}
