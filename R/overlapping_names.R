#' find overlapping names
findOverlappingNames <- function (names)
{
  unique_species_names <- names %>% dplyr::filter(language == "scientific" | language == "English") %>%
    dplyr::select(-dplyr::one_of("language", "source")) %>%
    dplyr::mutate(name = name %>% gsub(pattern = "-", replacement = " ") %>%
                    tolower()
    ) %>%
    .[rev(order(.$main)), ] %>%
    .[!duplicated(.[, c("name", "internalTaxonId")]), ]

  species_names_duplicates <- unique_species_names %>% dplyr::mutate(rowNum = c(1:nrow(.))) %>%
    .[rev(order(.$main)), ] %>%
    .[duplicated(.$name) | duplicated(.$name, fromLast = TRUE), ] %>%
    dplyr::filter(main == "FALSE")
  unique_species_names <- unique_species_names[!c(1:nrow(unique_species_names)) %in%
                                                 species_names_duplicates$rowNum, ]
  internal_taxon_ids <- unique(names$internalTaxonId)
  species_names_to_search <- tidyr::tibble(name = character())
  for (i in 1:length(internal_taxon_ids)) {
    species_names <- unique_species_names[unique_species_names$internalTaxonId ==
                                            internal_taxon_ids[i], ]
    overlapping_names <- overlappingNames(species_names$name)
    species_names_to_search <- dplyr::bind_rows(species_names_to_search,
                                                tidyr::tibble(internalTaxonId = internal_taxon_ids[i],
                                                              name = species_names$name[!overlapping_names] %>%
                                                                tolower() %>%
                                                                tm::removePunctuation(),
                                                              main = unique_species_names[unique_species_names$internalTaxonId == internal_taxon_ids[i], ] %>%
                                                                .[!duplicated(.$name), ] %>%
                                                                .[!overlapping_names, ] %>% .$main
                                                )
    )
  }
  search_terms <- tidyr::tibble(positiveSearchTerms = list(),
                                negativeSearchTerms = list())
  for (i in 1:length(internal_taxon_ids)) {
    search_terms <- findNegativeTerms(search_terms, species_names_to_search,
                                      internal_taxon_ids[i])
  }
  return(search_terms)
}

#' overlapping names
overlappingNames <- function (name)
{
  species_adist <- adist(x = name, partial = TRUE, ignore.case = TRUE)
  overlapping_names <- apply(species_adist, FUN = function(X) length(which(X ==
                                                                             0)) > 1, MARGIN = 2)
  return(overlapping_names)
}

#' find negative terms
findNegativeTerms <- function (search_terms, species_names_to_search, internal_taxon_id)
{
  species_names <- species_names_to_search$name[species_names_to_search$internalTaxonId ==
                                                  internal_taxon_id]
  names_contained_in <- species_names_to_search$name[species_names_to_search$internalTaxonId !=
                                                       internal_taxon_id] %>% .[grepl(pattern = paste0(species_names,
                                                                                                       collapse = "|"), x = .)] %>% .[!overlappingNames(.)]
  species_search_terms <- tidyr::tibble(internalTaxonId = internal_taxon_id,
                                        positiveSearchTerms = list(species_names), negativeSearchTerms = list(names_contained_in))
  search_terms <- dplyr::bind_rows(search_terms, species_search_terms)
  return(search_terms)
}
