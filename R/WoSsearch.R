#-------------------------------------------------------------------------#
#----------------------------- WoS SEARCH  -------------------------------#
#-------------------------------------------------------------------------#

# WoS.SEARCH
# uses RSelenium and rvest to search Web of Science for a search term and imports the BibTex files to a chosen directory

# author: alice stuart | date modified: 2020-03-13
# compiled in R version 3.6.3 (2020-03-12) -- "Holding the Windsock" running x86_64-apple-darwin15.6.0

# CURRENT PROBLEMS : cannot cope with no search results
#' @export
WoS.search <- function(searchterms,searchids,directory){
  unlink(directory, recursive = TRUE)
  eCaps <- list(chromeOptions = list(
    args = c('--headless', '--disable-gpu', '--window-size=1280,800'),
    prefs = list("download.default_directory" = directory)))
  rD <- RSelenium::rsDriver(extraCapabilities = eCaps)
  remDr <- rD[["client"]]
  hits <- c()
  for(i in 1:length(searchids)){
    remDr$navigate('https://apps.webofknowledge.com/WOS_GeneralSearch_input.do?product=WOS&search_mode=GeneralSearch&SID=C6F9yGsrRFFmiEirdH2&preferencesSaved=')
    Sys.sleep(0.5)
    # Post search term
    search_box <- remDr$findElement(using = 'id', "value(input1)")
    search_box$clearElement()
    search_box$sendKeysToElement(list(searchterms[i]))
    Sys.sleep(1)
    # click search
    findandclick(remDr, "/html/body/form[1]/div[1]/div/div/div/table/tbody/tr/td[3]/span/span[1]/button")
    # check page has changed
    search_box <- remDr$findElements(using = 'id', "value(input1)")
    if(length(search_box)==0){
    # read no. hits
    hits[i] <- remDr$findElement(using = 'xpath', '//*[@id="hitCount.top"]')
    hits[i] <- as.numeric(unlist(hits[i]$getElementText()))
    # export
    WoS.export(remDr,hits[i])
    }
  }
  remDr$close()
  rD[["server"]]$stop()
  rm(rD)
  gc()
  message(paste('files can be found in ',directory,sep='',collapse=''))
  return(hits)
}

#WoS.EXPORT
# navigates downloading from export menu
#' @export
WoS.initialexportsetup <- function(remDr){
  # click export button
  findandclick(remDr,'//*[@id="exportTypeName"]')
  # click other export options
  findandclick(remDr,'/html/body/div[1]/div[26]/div[2]/div/div/div/div[2]/div[3]/div[3]/div[2]/div[1]/ul/li/span/ul/li[3]/a')
  # click File Format dropdown
  findandclick(remDr,'//*[@id="select2-saveOptions-container"]')
  # click BibTeX
  bibtex <- remDr$findElement(using = 'xpath','//*[@id="select2-saveOptions-results"]')
  loc <- bibtex$getElementLocation()
  ypos <- round(as.numeric(loc$y)/8)
  remDr$mouseMoveToLocation(webElement = bibtex)
  remDr$mouseMoveToLocation(y = -2*ypos)
  remDr$click()
  # click Record Content dropdown
  findandclick(remDr,'//*[@id="select2-bib_fields-container"]')
  # click Full Record
  box <- remDr$findElement(using = 'xpath','//*[@id="select2-bib_fields-results"]')
  remDr$mouseMoveToLocation(webElement = box)
  remDr$click()
  # check 'Records from:' box
  findandclick(remDr,'//*[@id="numberOfRecordsRange"]')
}
#' @export
WoS.subsequentexportsetup <- function(remDr){
  Sys.sleep(0.1)
  # click export button
  findandclick(remDr,'/html/body/div[1]/div[26]/div[2]/div/div/div/div[2]/div[3]/div[3]/div[2]/div[1]/button')
  # click Record Content dropdown
  findandclick(remDr,'//*[@id="select2-bib_fields-container"]')
  # click Full Record
  box <- remDr$findElement(using = 'xpath','//*[@id="select2-bib_fields-results"]')
  remDr$mouseMoveToLocation(webElement = box)
  remDr$click()
  # check 'Records from:' box
  findandclick(remDr,'//*[@id="numberOfRecordsRange"]')
}
#' @export
WoS.export <- function(remDr,hits){
  # check what state WoS is in, choose exportsetup accordingly
  findandclick(remDr,'//*[@id="exportTypeName"]')
  length <- remDr$findElements(using = 'xpath','/html/body/div[1]/div[26]/div[2]/div/div/div/div[2]/div[3]/div[3]/div[2]/div[1]/ul/li/span/ul/li[3]/a')
  findandclick(remDr,'/html/body/div[11]/div[1]/button/span[1]')
  if(length(length)!=0){
    WoS.initialexportsetup(remDr)
  } else {
    WoS.subsequentexportsetup(remDr)
  }
  findandclick(remDr,'//*[@id="exportButton"]')
  checkandcloseexportmessage(remDr)
  if(hits >= 500){
    WoS.subsequentexportsetup(remDr)
    i<-1
    if(hits>=1000){
      for(i in 2:floor(hits/500)){
        # click in records from box
        keystosearchbox(remDr = remDr,xpath = '//*[@id="markFrom"]',keys=as.character(i*500-499))
        keystosearchbox(remDr = remDr,xpath = '//*[@id="markTo"]',keys=as.character(i*500))
        findandclick(remDr,'//*[@id="exportButton"]')
        checkandcloseexportmessage(remDr)
      }
    }
    keystosearchbox(remDr = remDr,xpath = '//*[@id="markFrom"]',keys=as.character(i*500+1))
    keystosearchbox(remDr = remDr,xpath = '//*[@id="markTo"]',keys=as.character(hits))
    findandclick(remDr,'//*[@id="exportButton"]')
    checkandcloseexportmessage(remDr)
  }
}
#' @export
findandclick <- function(remDr,xpath){
  element <- remDr$findElement(using = 'xpath',xpath)
  element$clickElement()
}
#' @export
findandclickid <- function(remDr,id){
  element <- remDr$findElement(using = 'id',id)
  element$clickElement()
}
#' @export
findandclickcss <- function(remDr,css){
  element <- remDr$findElement(using = 'css',css)
  element$clickElement()
}
#' @export
keystosearchbox <- function(remDr,xpath,keys){
  box <- remDr$findElement(using = 'xpath',value = xpath)
  box$clearElement()
  box$sendKeysToElement(list(keys))
}
#' @export
checkandcloseexportmessage <- function(remDr){
  Sys.sleep(0.5)
  box <- remDr$findElements(using = 'xpath',value = '/html/body/div[11]/div[1]/button/span[1]')
  if(length(box)!=0){
    findandclick(remDr,'/html/body/div[11]/div[1]/button/span[1]')
  }
}
#' @export
importrefs <- function(directory){
  current_wd <- getwd()
  setwd(directory)
  files <- list.files(pattern = ".bib$")
  references <- lapply(files, revtools::read_bibliography)
  result <- Reduce(function(...) merge(..., all=T), references)
  setwd(current_wd)
  reference_id <- paste('p',c(1:nrow(result)),sep = '')
  result <- cbind(reference_id,result)
  return(result)
}
