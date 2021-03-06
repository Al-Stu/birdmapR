#-------------------------------------------------------------------------#
#---------------------------- EOL URLSCRAPER  ----------------------------#
#-------------------------------------------------------------------------#

# EOL.URLSCRAPER
# this function enters each value in search_terms into the eol search bar and returns the links and text from the resultant dropdown menu
# it returns a dataframe with link text and link for each search term, will return NA if no curated results for search
# requires Rselenium


# author: alice stuart | date modified: 2020-03-11
# compiled in R version 3.6.3 (2020-02-29) -- "Holding the Windsock" running x86_64-apple-darwin15.6.0
#' @export
EOL.urlscraper <- function(latin_names,internalTaxonId,best_match){
  eCaps <- list(chromeOptions = list(args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
  rD <- RSelenium::rsDriver(extraCapabilities = eCaps)
  remDr <- rD[["client"]]
  text <- c()
  link <- c()
  url <- cbind(text,link)
  for(i in 1:length(latin_names)){
    NA_template <- data.frame(internalTaxonId = internalTaxonId[i],
                              text = NA,
                              link = NA)
    remDr$navigate('https://eol.org/')
    Sys.sleep(0.5)
    # Navigate to search box
    option <- remDr$findElement(using = 'xpath', "/html/body/div[2]/form")
    option$clickElement()
    Sys.sleep(0.5)
    # Post search term
    search_box <- remDr$findElement(using = 'css selector', "#q")
    search_box$sendKeysToElement(list(latin_names[i]))
    Sys.sleep(1)
    # navigate to dropdown
    drop_down <- remDr$findElements(using = 'xpath', "/html/body/div[2]/form/span/div/div/div")
    if(length(drop_down)==1){
      url <- rbind(url,extractfromhyperlink(x_path = "/html/body/div[2]/form/span/div/div/div/a",remDr = remDr,id = internalTaxonId[i]))
    } else if (length(drop_down)>1){
      x_path <- vector(mode = 'character',length = length(drop_down))
      for(j in 1: length(drop_down)){
        x_path[j] <- paste('/html/body/div[2]/form/span/div/div/div[',j,']/a',sep='',collapse='')
      }
      dropdown_options <- extractfromhyperlink(x_path = x_path,remDr = remDr,id = internalTaxonId[i])
      if(best_match == TRUE){
        option_no <- fuzzymatchposition(latin_names[i],dropdown_options[,1])
        if(length(option_no)>1){
          url <- rbind(url, NA_template)
        } else {
          url <- rbind(url,dropdown_options[option_no,])
        }
      } else {
        url <- rbind(url,dropdown_options)
      }
    } else if (length(drop_down)==0){
      url <- rbind(url, NA_template)
    } else {
      warning('something is wrong!')
    }
  }
  remDr$close()
  rD[["server"]]$stop()
  rm(rD)
  gc()
  url <- unique(url[!is.na(url$link),])
  rownames(url) <- c(1:nrow(url))
  if(length(duplicated(url$link))>0){
    warning('some urls have been assigned to more than one species, these can be found using the duplicated() function and will need to be assessed separately')
  }
  return(url)
}

# EXTRACTFROMHYPERLINK
# this function extracts the link text and url from an EOL dropdown list, used in eolurlscraper
# it returns a dataframe with link text and link for each entry in the EOL dropdown list

#' @export
extractfromhyperlink <- function(x_path,remDr,id){
  text <- c()
  link <- c()
  for(i in 1:length(x_path)){
    dropdown_element <- remDr$findElement(using = 'xpath', x_path[i])
    text[i] <- dropdown_element$getElementText()
    link[i] <- dropdown_element$getElementAttribute(attrName = 'href')
  }
  result <- cbind.data.frame(rep_len(id,length(x_path)),as.character(unlist(text)),as.character(unlist(link)))
  names(result) <- c('internalTaxonId','text','link')
  return(result)
}

#' @export
closerD <- function(remDr,rD){
  remDr$close()
  rD[["server"]]$stop()
  rm(rD)
  gc()
}

# EOL.DEDUPLICATE
#' @export
EOL.deduplicate <- function(EOLurls,formattedIOC){
  duplicates <- EOLurls[duplicated(EOLurls$text) | duplicated(EOLurls$text, fromLast = TRUE),]
  if(nrow(duplicates)==0){
    message('no duplicates found')
    result <- duplicates
  } else {
    ids <- unique(duplicates$internalTaxonId)
    duplicate_names <- formattedIOC[formattedIOC$internalTaxonId==ids,]
    contained <- c()
    for (i in 1:length(ids)) {
      id_names <- duplicate_names$name[duplicate_names$internalTaxonId==ids[i]]
      id_search <- paste(id_names, sep = '|', collapse = '|')
      positions <- unlist(stringr::str_locate_all(string = duplicate_names$name[duplicate_names$internalTaxonId!=ids[i]], pattern = id_search))
      if(length(positions)==0){
        contained[i] <- FALSE
      } else {
        contained[i] <- TRUE
      }
    }
    contained_ids <- ids[contained]
    duplicate_rows <- as.numeric(rownames(duplicates[duplicates$internalTaxonId==contained_ids,]))
    result <- EOLurls[-duplicate_rows,]
  }
  if(nrow(result)!=length(unique(EOLurls$text))){
    stop('error: removed urls that were not duplicates')
  }
  rownames(result) <- c(1:nrow(result))
  return(result)
}
