#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####################  FUNCTION TO EXCLUDE MEASUREMENTS BASED ON DRASTIC CHANGES IN THE LOD  ###################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function harmonizes excludes the measurements from study year for chemicals that show drastic
#          changes in the LOD. We do not want our analyses to be affect by technological advances. 
#
# Inputs: dataset_unclean - dataframe of the unclean dataset
#         dataset_doc_cleaning - dataframe of the dataset containing the cleaning documentation
#
# Outputs: dataset_cleaned - dataframe of the dataset with measurements in affected study year are defined as 
#                            missing.

exclude_measurements_based_changes_in_lod <- function(dataset_unclean
                                                      , dataset_doc_cleaning)
{
  # Define a subset to indicate which study years for which chemicals should be excluded based on 
  # drastic changes in the LOD.
  subset_to_exclude <- dataset_doc_cleaning %>%
    filter(remove_cycle_changes_in_LOD == 1)
  # View(subset_to_exclude)
  
  # Determine the codename of chemicals in which some of study years need to be excluded
  chemicals_with_exclusion <- subset_to_exclude %>%
    pull(variable_codename_use) %>%
    unique(.) #%>%
    # .[1]
  # print(chemicals_with_exclusion)
  
  # chemicals_with_exclusion <- c("LBX138"
  #                               , "LBX138LA")
  
  
  # Determine number of chemicals that have study years that need to be excluded
  num_chemicals_with_exclusion <- length(chemicals_with_exclusion)

  
  for(i in seq(num_chemicals_with_exclusion))
  {
    # Determine the codename of a chemical with the needed exclusion
    chemical_exclude_i <- chemicals_with_exclusion[i]
    # print(chemical_exclude_i)

    # Extract the row pertaining to the codename
    subset_doc_cleaning_i <- subset_to_exclude %>%
      filter(variable_codename_use == chemical_exclude_i)
    # print(subset_doc_cleaning_i)

    # Determine which study years should be excluded
    cycles_to_exclude <- subset_doc_cleaning_i %>%
      pull(SDDSRVYR)
    # print(cycles_to_exclude)

    # Determine the row indices in the nhanes dataset for participants who have measurements in
    # these affected study years
    index_cycles_to_exclude <- which(dataset_unclean[,"SDDSRVYR"] %in% cycles_to_exclude)
    # print(index_cycles_to_exclude)

    # For participants with measurements in the affected years, their measurements will be set to NA as
    # to exclude these measurements from the analysis
    dataset_unclean[index_cycles_to_exclude, chemical_exclude_i] <- NA


    # # Check if the measurements are now NA after the exclusions
    # View(dataset_unclean %>%
    #        select(all_of(chemical_exclude_i)
    #               , SDDSRVYR) %>%
    #        unique(.))
    
    subset_doc_chem_include_i <- dataset_doc_cleaning %>%
      filter(variable_codename_use == chemical_exclude_i) %>%
      filter(is.na(remove_cycle_changes_in_LOD) == TRUE)
    
    cycles_doc_include_i <- subset_doc_chem_include_i %>%
      pull(SDDSRVYR) %>%
      unique(.)
    
    subset_chem_include_i <- dataset_unclean %>%
      select(all_of(chemical_exclude_i)
             , SDDSRVYR) %>%
      na.omit(.)
    
    cycles_chem_include_i <- subset_chem_include_i %>%
      pull(SDDSRVYR) %>%
      unique(.)
    
    missing_cycles <- outersect(cycles_doc_include_i
                                , cycles_chem_include_i)
    
    no_missing_cycles <- is_empty(missing_cycles)
    
    if(no_missing_cycles == FALSE)
    {
      
      subset_doc_original_i <- dataset_doc_cleaning %>%
        filter(variable_codename == chemical_exclude_i) %>%
        filter(SDDSRVYR %in% missing_cycles)
      # print(subset_doc_original_i)

      codename_change_i <- subset_doc_original_i %>%
        pull(codename_change) %>%
        is_empty(.)
      # print(codename_change_i)
      
      if(codename_change_i == FALSE)
      {
        index_cycles_missing <- which(dataset_unclean[,"SDDSRVYR"] %in% missing_cycles)
        
        dataset_unclean[index_cycles_missing, chemical_exclude_i] <- NA
      }
      
      subset_chem_include_i <- dataset_unclean %>%
        select(all_of(chemical_exclude_i)
               , SDDSRVYR) %>%
        na.omit(.)
      
      cycles_chem_include_i <- subset_chem_include_i %>%
        pull(SDDSRVYR) %>%
        unique(.)
      # print(cycles_chem_include_i)
      
      missing_cycles <- outersect(cycles_doc_include_i
                                  , cycles_chem_include_i)
      
      no_missing_cycles <- is_empty(missing_cycles)
      
      if(no_missing_cycles == FALSE)
      {
        print(chemical_exclude_i)
        print(cycles_doc_include_i)
        print(cycles_chem_include_i)
        print(missing_cycles)
      }
      
    }
  }
  
  
  
  # Define the dataset as cleaned after measurements in the affected study years are excluded
  dataset_cleaned <- dataset_unclean

  return(dataset_cleaned)
}