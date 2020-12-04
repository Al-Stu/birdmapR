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

bird_search_terms <- all_names %>%
  birdDataScrapeR::regexSearchTerms() %>%
  dplyr::left_join(overlap_checked_names) %>%
  dplyr::left_join(all_names[ , c('internalTaxonId', 'scientificName')]) %>%
  unique()

bird_search_terms$regex_sci[bird_search_terms$scientificName == "Vidua codringtoni"] <- "vidua codringtoni|vidua chalybeata codringtoni|v codringtoni|v chalybeata codringtoni|v c codringtoni"

bird_search_terms$regex_both[bird_search_terms$scientificName == "Vidua codringtoni"] <- paste0(bird_search_terms$regex_common[bird_search_terms$scientificName == "Vidua codringtoni"],
                                                                                                "|",
                                                                                                bird_search_terms$regex_sci[bird_search_terms$scientificName == "Vidua codringtoni"])
usethis::use_data(bird_search_terms, overwrite = TRUE)
