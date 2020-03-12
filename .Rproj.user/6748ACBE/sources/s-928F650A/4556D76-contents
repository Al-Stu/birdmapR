## IOC_v10.1

# download raw xlsx
IOC_v10.1_raw <- IOC.download(version = '10.1')
# format using IOC.format
IOC_v10.1_formatted <- IOC.format(IOC_v10.1_raw)

## otidiformes data
# import and clean RL otidiformes data (downloaded all possible RL files for otifidormes taxon search)
otidiformes_data_raw <- importcsvlist(directory = '/Users/Alice/Downloads/redlist_species_data_b418d010-bba7-4dc7-85ca-69a5c1c338a0',pattern='')
otidiformes_data <- RL.clean(otidiformes_data_raw)

# add IOC names
otidiformes_data[['IOC_altnames']] <- IOC.altnames(formattedIOC = IOC_v10.1_formatted, cleaned_redlist_data = otidiformes_data)

# add EOL urls
# best_match == TRUE only returns the closest match to original searchterm from each dropdown
otidiformes_data[['EOL_urls_bestmatch']] <- eolurlscraper(latin_names = otidiformes_data[['IOC_altnames']]$scientificName,internalTaxonId = otidiformes_data[['IOC_altnames']]$internalTaxonId,best_match = TRUE)
otidiformes_data[['EOL_urls_bestmatch']] <- unique(otidiformes_data[['EOL_urls_bestmatch']][!is.na(otidiformes_data[['EOL_urls_bestmatch']]$link),])

# best_match != TRUE returns the text and link from every element of dropdown list, able to scrape names not on the IOC list but has the potential to associate subspecies to the wrong IUCN species
otidiformes_data[['EOL_urls_all']] <- eolurlscraper(latin_names = otidiformes_data[['IOC_altnames']]$scientificName,internalTaxonId = otidiformes_data[['IOC_altnames']]$internalTaxonId,best_match = FALSE)
otidiformes_data[['EOL_urls_all']] <- unique(otidiformes_data[['EOL_urls_all']][!is.na(otidiformes_data[['EOL_urls_all']]$link),])

# add EOL common names
otidiformes_data[['EOL_common_names']] <- EOL.commonnamescraper(otidiformes_data[['EOL_urls_all']])

# add EOL alternate scientific names
otidiformes_data[['EOL_sci_names']] <- EOL.scinamescraper(otidiformes_data[['EOL_urls_all']])


usethis::use_data(IOC_v10.1_raw,IOC_v10.1_formatted, overwrite = TRUE)
