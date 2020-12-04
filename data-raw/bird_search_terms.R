## code to prepare `bird_search_terms` dataset goes here
library(birdDataScrapeR)
library(tidyRedlist)
library(magrittr)

all_names <- tidyRedlist::tidied_all_birds %>%
  getIOCNames() %>%
  .$names %>%
  dplyr::mutate(name = name %>%
                  tolower()
                )

overlap_checked_names <- findOverlappingNames(all_names)
usethis::use_data(overlap_checked_names, overwrite = TRUE)

bird_search_terms <- tidyr::unnest(overlap_checked_names[ , c(3,1)],
                              positiveSearchTerms) %>%
  dplyr::rename(name = positiveSearchTerms) %>%
  dplyr::left_join(all_names[ , c('internalTaxonId', 'name', 'language')]) %>%
  unique() %>%
  dplyr::mutate(language = ifelse(is.na(language),
                           "English",
                           language
                           )
         ) %>%
  birdDataScrapeR::regexSearchTerms() %>%
  dplyr::left_join(overlap_checked_names) %>%
  dplyr::left_join(all_names[ , c('internalTaxonId', 'scientificName')]) %>%
  unique()

usethis::use_data(bird_search_terms, overwrite = TRUE)
