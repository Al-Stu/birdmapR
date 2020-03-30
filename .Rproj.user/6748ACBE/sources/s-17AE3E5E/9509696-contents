#-------------------------------------------------------------------------#
#---------------------------- EOL NAMESCRAPER  ----------------------------#
#-------------------------------------------------------------------------#

# EOLSCINAMESCRAPER
# this function takes urls for species homepages and scrapes the alternate latin names for each spp
# it returns a list with one element for each spp, containing a dataframe of all EOL latin names
# requires rvest

# author: alice stuart and matt lewis | date modified: 2020-03-11
# compiled in R version 3.6.3 (2020-02-29) -- "Holding the Windsock" running x86_64-apple-darwin15.6.0
#' @export
EOL.scinamescraper <- function(EOL_urls){
  name_url <- c()
  latin_names_df <- list()
  for(l in 1:length(EOL_urls$link)){
    name_url[l] <- paste(EOL_urls$link[l],'/names',sep='',collapse='')
    alt_name_text <- EOL.textboxfromurl(urls = name_url[l], x_path = '/html/body/div[3]/div[2]/div/div[1]/div[2]/div/div[2]')
    if(is.null(alt_name_text)==FALSE){
      kept_alt_names<-c()
      for(i in 1:length(alt_name_text)){
        if(grepl("Synonym|Reference|according", alt_name_text[i]) == F){
          name <- unlist(strsplit(alt_name_text[i],split=" "))
          if(length(name)<=2){
            name <- paste(name, collapse = ' ')
          } else if(name[3]==tolower(name[3])){
            name <- paste(name[1:3],collapse=" ")
          }else{
            name <- paste(name[1:2],collapse=" ")
          }
          kept_alt_names <- c(kept_alt_names,name)
        }
      }
      kept_sci_names <- c(as.character(EOL_urls$text[l]),kept_alt_names)
      latin_names_df[[l]] <- data.frame(name=kept_alt_names,type="scientific",language="latin")
      latin_names_df[[l]] <- unique(latin_names_df[[l]])
      latin_names_df[[l]]$main <- c('true',rep_len('false',(nrow(latin_names_df[[l]])-1)))
    }  else {
      latin_names_df[[l]] <- data.frame(name=EOL_urls$text[l],type="scientific",language="latin",main='true')
    }
  }
  latin_names <- EOL.nameslisttodf(latin_names_df,EOL_urls$internalTaxonId)
  source <- rep_len('EOL', nrow(latin_names))
  latin_names <- cbind(latin_names[,c(1,2,5)],source)
  return(latin_names)
}

# EOLCOMNAMESCRAPER
# as above but common names
#' @export
EOL.commonnamescraper <- function(EOL_urls){
  name_url <- c()
  common_names_df <- list()
  for(l in 1:length(EOL_urls$link)){
    name_url[l] <- paste(EOL_urls$link[l],'/names',sep='',collapse='')
    page <- xml2::read_html(name_url[l])
    common_name_nodes <- rvest::html_nodes(page, xpath='/html/body/div[3]/div[2]/div/div[2]/div[1]/div/div[2]/div/div')
    common_name_list <- as.list(as.character(rvest::html_text(rvest::html_children(common_name_nodes))))
    names_template <- data.frame(name=character(),type=character(), main = factor(levels = c('true','false')))
    if(length(common_name_list)!=0)  {
      for(i in 1:length(common_name_list)){
        names <- unlist(strsplit(common_name_list[[i]][1],split="\n"))
        preferred <- grepl('preferred',names)
        names <- names[names != "" & grepl("Recognize|recognize|prefer",names)==F]
        if(i==1){
          common_names_df[[l]] <- names_template
        }
        if(length(names)!=0){
          temp_main <- rep_len(any(preferred),length(names))
          temp_type <- rep_len('common',length(names))
          temp_df <- cbind(as.character(names),temp_type,temp_main)
          colnames(temp_df) <- c('name','type','main')
          common_names_df[[l]]<- rbind(common_names_df[[l]],temp_df)
        }
      }
      common_names_df[[l]] <-unique(common_names_df[[l]])
    } else {
      common_names_df[[l]] <- data.frame(name=NA,type=NA)
    }
  }
  common_names <- EOL.nameslisttodf(common_names_df,EOL_urls$internalTaxonId)
  language <- rep_len('English', nrow(common_names))
  common_names <- cbind(common_names[,c(1,2)],language,common_names[,c(4,5)])
  common_names$name <- stringr::str_trim(as.character(common_names$name))
  return(common_names)
}

# EOL.TEXTBOXFROMURL
# reads a text box from specified url and xpath
#' @export
EOL.textboxfromurl <- function(urls, x_path){
  page <- xml2::read_html(urls)
  # Alternative names
  alt_name_nodes <- rvest::html_nodes(page,xpath = x_path)
  alt_name_text <- unlist(strsplit(rvest::html_text(alt_name_nodes),split="\n"))
  alt_name_text <- alt_name_text[which(alt_name_text != "")]
  return(alt_name_text)
}

# EOL.NAMELISTTODF
#' @export
EOL.nameslisttodf <- function(nameslist, ids){
  names <- c()
  for(i in 1:length(nameslist)){
    internalTaxonId <- rep_len(ids[i],
                               nrow(nameslist[[i]]))
    temp_df <- NULL
    temp_df <- cbind(internalTaxonId,nameslist[[i]])
    if(length(temp_df)!=3){
      if(length(names)<1){
        names <- temp_df
      } else {
        names <- rbind(names,temp_df)
      }
    }
  }
  names$source <- rep_len('EOL',nrow(names))

  return(names)
}
