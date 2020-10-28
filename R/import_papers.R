###### PDF HANDLING ##########


#' Match pdfs with their references
#'
#' Read pdfs as text and associate with references, pdfs must have been saved with
#' paper title within file title
#'
#' @param PDFs a list containing the text for one pdf per element, can be created with
#' \code{\link{importPDFs}}
#' @param references_file file path to .RIS file containing references
#'
#' @return tibble with references and title of PDF they've been matched to
#'
#' @export

matchPDFsToRefs <- function(PDFs,
                            references_file){

  # import the references
  references <- revtools::read_bibliography(references_file)
  references$paper_id <- paste0('p', c(1:length(references$label)))

  # match paper titles to pdf titles
  matched_titles <- fuzzymatchpairs(x = references$title, y = names(paper_texts))

  references <- mutate(references,
                       pdf = matched_titles$y.name,
                       pdf_position = matched_titles$y.i)

  return(references_file)
}

#' import PDFs
#'
#' import all pdfs in a folder
#'
#' @param pdf_location path to folder containing pdf files to be imported
#'
#' @return a list with one element per pdf, element names are the original file names,
#' within each element, there is an object per page

importPDFs <- function(pdf_location){
  # get original working directory
  original_wd <- getwd()

  # list file names
  setwd(pdf_location)
  files <- list.files(pattern = "pdf$")

  # pdfs must have been saved with the paper title in their file title
  paper_text <- lapply(files, pdftools::pdf_text)
  names(paper_text) <- files
  setwd(original_wd)

  return(paper_text)
}

#' clean pdfs for analysis
#'
#' Cleans some common features from pdf texts
#'
#' @param paper_texts a list of paper texts, can be imported using \code{\link{importPDFs}}
#' @param remove_post_text logical, if true all text after (and including) acknowledgements,
#' bibliograpy and references will be removed (but only if only said once)
#'
#' @return tibble with one row per paper and columns \code{paper_name} with names from list
#' \code{paper_texts}; \code{original_paper_text} with the original text from pdf;
#' \code{cleaned_paper_text} with a cleaned version of the paper text ready for text mining
#' analysis; and \code{cleaned_paper_text_lower_case} with a lower case version of the cleaned
#' paper text.
#'
#' @export
#'
cleanTexts <- function(paper_texts, remove_post_text = T, remove_stop_words = T){
  paper_text <- tidyr::tibble(paper_name = names(paper_texts),
                              original_paper_text = lapply(paper_texts,
                                                           function(X) paste(X,
                                                                             sep = ' ',
                                                                             collapse = ' '
                                                                             )
                                                           ) %>%
                                unlist(),
                              cleaned_paper_text = iconv(original_paper_text, to = 'ASCII') %>%  # convert to ascii characters
                                textclean::replace_white() %>%
                                tm::stripWhitespace() %>%  # remove white space
                                tm::removePunctuation() %>%   # remove punctuation
                                ifelse(test = substring(.,1,1)=="c", # remove copyright c from start if present
                                       yes = substring(.,2),
                                       no = .) %>%
                                tm::removeNumbers()
                              )

  if(remove_stop_words){
    paper_text$cleaned_paper_text <- removeWords(paper_text$cleaned_paper_text, stopwords("english"))
  }

  if(remove_post_text){
    paper_text$cleaned_paper_text <- removePostText(paper_text$cleaned_paper_text)
  }

  # create column with all lower case
  paper_text$cleaned_paper_text_lower_case <- tolower(paper_text$cleaned_paper_text)

  return(paper_text)
}

#' remove post text
#'
#' remove all text after (and including) acknowledgements, bibliograpy and references (but only if only said once)
#'
#' @param paper_text character vector with one paper text per element
#'
#' @return paper text with any post text removed
#'
removePostText <- function(paper_text){
  for(i in 1:length(paper_text)){
    if(length(stringr::str_locate_all(all_lower[i],  "(A|a)cknowledgement")$start)==1){
      paper_text[i] <- gsub(x = paper_text[i],  pattern = "(A|a)cknowledgement(.*?)$", ignore.case = TRUE, replacement = '')
    }
    if(length(str_locate_all(all_lower[i],  "(B|b)ibliography")$start)==1){
      paper_text[i] <- gsub(x = paper_text[i],  pattern = "(B|b)ibliography(.*?)$", ignore.case = TRUE, replacement = '')
    }
    if(length(str_locate_all(all_lower[i],  "(R|r)eferences")$start)==1){
      paper_text[i] <- gsub(x = paper_text[i],  pattern = "(R|r)eferences(.*?)$", ignore.case = TRUE, replacement = '')
    }
  }
  return(paper_text)
}
