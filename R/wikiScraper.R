#-------------------------------------------------------------------------#
#--------------------------- WIKIPEDIA SCRAPER ---------------------------#
#-------------------------------------------------------------------------#


#' scrape all text for a vector of species from wikipedia
#'
#' @param species a list of species names to be searched in wikipedia
#' @return a list containing the text from each section of the species' wikipedia
#' page
#'
#' @export
wikiScrape <- function(species){
  eCaps <- list(chromeOptions = list(
    args = c('--headless', '--disable-gpu', '--window-size=1280,800')
    )
  )
  rD <- RSelenium::rsDriver(extraCapabilities = eCaps)
  remDr <- rD[["client"]]
  text <- c()
  contents <- c()
  remDr$navigate('https://www.wikipedia.org/')
  keystosearchbox(remDr,'//*[@id="searchInput"]',species[1])
  findandclick(remDr, '/html/body/div[2]/form/fieldset/button/i')
  text[1] <- getElementText(remDr, 'xpath', '/html/body/div[3]/div[3]/div[4]/div')
  if(length(remDr$findElements('xpath', '//*[@id="toc"]'))!=0){
    contents[1] <- getElementText(remDr, 'xpath', '//*[@id="toc"]')
  } else {
    contents[1] <- NA
  }
  for(i in 2:length(species)){
    Sys.sleep(2)
    keystosearchbox(remDr, '//*[@id="searchInput"]', species[i])
    findandclick(remDr, '//*[@id="searchButton"]')
    text[i] <- getElementText(remDr, 'xpath', '/html/body/div[3]/div[3]/div[4]/div')
    if(length(remDr$findElements('xpath', '//*[@id="toc"]'))!=0){
      contents[i] <- getElementText(remDr, 'xpath', '//*[@id="toc"]')
    } else {
      contents[i] <- NA
    }  }
  closerD(remDr, rD)
  split_text <- splitWikiScrape(text = text, contents = contents)
  return(split_text)
}

#' Find an element and get text
#'
#' @param remDr
#' @param using
#' @param path
#' @return a value containing the text from the element
#'
getElementText <- function(remDr, using, path){
  page <- remDr$findElement(using, path)
  text  <- page$getElementText() %>%
    unlist()
  return(text)
}

#' Find an element and get text
#'
#' @param remDr
#' @param using
#' @param path
#' @return a value containing the text from the element
#'
splitWikiScrape <- function(text, contents){
  split_contents <- stringr::str_split(string = contents, pattern = '\\n') %>%
    sapply(function(x) gsub(pattern = '^[0-9][.]*[0-9]* ', replacement = '', x = x))
  split_texts <- list()
  for(i in 1:length(text)){
    if(!all(is.na(split_contents[[i]]))){
      pattern = paste('\\n', split_contents[[i]], '\\[', sep = '', collapse = '|')
    } else {
      pattern = paste('\\n', c('References|External links'), '\\[', sep = '', collapse = '|')
    }
    if(any(grepl(pattern = pattern, x = text[i]))){
      split_texts[[i]] <- stringr::str_split(string = text[i],
                                             pattern = pattern) %>%
        sapply(function(x) stringr::str_remove(string = x, pattern = '^edit]\\n')) %>%
        tm::stripWhitespace() %>%
        as.vector()
    } else {
      split_texts[[i]] <- stringr::str_split(string = text[i],
                                             pattern = paste('\\n',
                                                             split_contents[[i]],
                                                             '\\n',
                                                             sep = '',
                                                             collapse = '|')) %>%
        unlist() %>%
        tm::stripWhitespace() %>%
        as.vector()
    }
    if(!any(is.na(split_contents[[i]]))){
      names(split_texts[[i]]) <- split_contents[[i]]
      names(split_texts[[i]])[1] <- 'Introduction'
    } else {
      names(split_texts[[i]]) <-
        c('Introduction','References','External links')[1:length(split_texts[[i]])]
    }
  }
  return(split_texts)
}

#' Find an element and get text
#'
#' @param remDr
#' @param using
#' @param path
#' @return a value containing the text from the element
#'
#' @export
textListToTibble <- function(list, directory, name, internalTaxonId, scientificName){
  text_tibble <- tidyr::tibble(internalTaxonId = character(),
                               scientificName = character(),
                               section = character(),
                               text = character())
  for(i in 1:length(list)){
    names <- names(list[[i]])
    tibble <- tidyr::tibble(internalTaxonId = internalTaxonId[i],
                            scientificName = scientificName[i],
                            section = names,
                            text = list[[i]])
    text_tibble <- dplyr::bind_rows(text_tibble, tibble)
  }
  return(text_tibble)
}
