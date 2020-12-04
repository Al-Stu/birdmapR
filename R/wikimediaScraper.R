wikimediaThumbnailScraper <- function(search_terms, file_path){
  #eCaps <- list(chromeOptions = list(args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
  rD <- RSelenium::rsDriver() # extraCapabilities = eCaps
  remDr <- rD[["client"]]
  remDr$navigate('https://commons.wikimedia.org')
  Sys.sleep(0.5)
  findandclick(remDr, '//*[@id="searchButton"]')
  findandclick(remDr, '/html/body/div[3]/div[3]/div[4]/div[2]/form/div[4]/div[1]/span/a/span[3]')
  findandclick(remDr, '/html/body/div[3]/div[3]/div[4]/div[2]/form/div[4]/div[1]/div/div/fieldset[3]/div/div[1]/div/div/div/div/span/span[3]')
  findandclick(remDr, '/html/body/div[6]/div[13]/div[3]/span[3]')
  keystosearchbox(remDr, '//*[@id="ooui-45"]', paste(search_terms$negative[[1]], collapse = ", "))
  keystosearchbox(remDr, '//*[@id="ooui-php-1"]', paste0('"', paste(search_terms$positive[[1]], collapse = '" OR "'), '"'))
  findandclick(remDr, '/html/body/div[3]/div[3]/div[4]/div[2]/form/div[1]/div/div/div/span/span/button/span[2]')
  results <- remDr$findElement("xpath", '/html/body/div[3]/div[3]/div[4]/div[3]/ul')
}
