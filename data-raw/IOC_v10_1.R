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
otidiformes_data[['EOL_urls_bestmatch']] <- EOL.urlscraper(latin_names = otidiformes_data[['IOC_altnames']]$scientificName,internalTaxonId = otidiformes_data[['IOC_altnames']]$internalTaxonId,best_match = TRUE)

# best_match != TRUE returns the text and link from every element of dropdown list, able to scrape names not on the IOC list but has the potential to associate subspecies to the wrong IUCN species
otidiformes_data[['EOL_urls_all']] <- EOL.urlscraper(latin_names = otidiformes_data[['IOC_altnames']]$name,internalTaxonId = otidiformes_data[['IOC_altnames']]$internalTaxonId,best_match = FALSE)

# deduplicate EOL urls
otidiformes_data[['EOL_urls_deduplicated']] <- EOL.deduplicate(EOLurls = otidiformes_data[['EOL_urls_all']], formattedIOC = otidiformes_data[['IOC_altnames']])

# add EOL common names
otidiformes_data[['EOL_common_names']] <- EOL.commonnamescraper(otidiformes_data[['EOL_urls_deduplicated']])

# add EOL alternate scientific names
otidiformes_data[['EOL_sci_names']] <- EOL.scinamescraper(otidiformes_data[['EOL_urls_deduplicated']])

# combine all common names and sci names
otidiformes_data[['all_common_names']] <- combineandsort(names1 = otidiformes_data[['common_names']],names2 = otidiformes_data[['EOL_common_names']])
otidiformes_data[['all_sci_names']] <- combineandsort(names1 = otidiformes_data[['IOC_altnames']], names2 = otidiformes_data[['EOL_sci_names']])

# make dataframe of common name and sci name search terms for each spp
otidiformes_data[['search_terms']] <- createsearchterms(all_common_names = otidiformes_data[['all_common_names']],all_scientific_names = otidiformes_data[['all_sci_names']])

# WoS search
WoS.search(searchterms = otidiformes_data[['search_terms']]$WoS,searchids = otidiformes_data[['search_terms']]$internalTaxonId,directory = '/Users/Alice/Documents/Masters/WoS_results')

# Import refs to R
paper_data <- list()
paper_data[['references']] <- importrefs(directory = '/Users/Alice/Documents/Masters/WoS_results')

usethis::use_data(IOC_v10.1_raw,IOC_v10.1_formatted,otidiformes_data_raw,otidiformes_data, overwrite = TRUE)
