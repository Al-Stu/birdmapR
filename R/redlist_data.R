#-------------------------------------------------------------------------#
#---------------------------- RED LIST DATA  -----------------------------#
#-------------------------------------------------------------------------#

# RL.IMPORT
# import and neaten RL data

# author: alice stuart | date modified: 2020-03-11
# compiled in R version 3.6.3 (2020-03-12) -- "Holding the Windsock" running x86_64-apple-darwin15.6.0
# merge dataframes with one line per species
RL.clean <- function(species_data){
  species_data[['assessments']] <- merge(species_data[['assessments']], species_data[['simple_summary']], all = TRUE)
  species_data[['assessments']] <- merge(species_data[['assessments']], species_data[['taxonomy']], all = TRUE)
  species_data[['assessments']] <- merge(species_data[['assessments']], species_data[['all_other_fields']], all = TRUE)

  species_data[['simple_summary']] <- NULL
  species_data[['taxonomy']] <- NULL
  species_data[['all_other_fields']] <- NULL

  # rename assessments to species
  names(species_data)[grep(pattern = 'assessments',x = names(species_data))] <- 'species_data'

  # add source column to and remove scientificName from common names
  species_data[['common_names']]$source <- rep_len('RL',nrow(species_data[['common_names']]))
  species_data[['common_names']] <- merge(species_data[['species_data']][,c(1,2)],species_data[['common_names']])
  species_data[['common_names']] <- species_data[['common_names']][,-1]
  return(species_data)
}
