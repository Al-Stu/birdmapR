## code to prepare `bird_search_terms` dataset goes here
library(birdDataScrapeR)
library(tidyRedlist)
library(magrittr)

all_names <- tidyRedlist::tidied_all_birds %>%
  getIOCNames() %>%
  .$names %>%
  dplyr::mutate(name = name %>%
                  tolower() %>%
                  gsub(pattern = '-', replacement = ' '
                  )
                )

overlap_checked_names <- findOverlappingNames(all_names)

bird_search_terms <- tidyr::unnest(overlap_checked_names[ , c(3,1)],
                              positiveSearchTerms) %>%
  dplyr::rename(name = positiveSearchTerms) %>%
  dplyr::left_join(all_names[ , c('internalTaxonId', 'name', 'language')]) %>%
  birdDataScrapeR::regexSearchTerms() %>%
  dplyr::left_join(overlap_checked_names) %>%
  dplyr::left_join(all_names[ , c('internalTaxonId', 'scientificName')])

usethis::use_data(bird_search_terms, overwrite = TRUE)
