#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###############  FUNCTION TO INDICATE RELATION TO THE LOD WHEN THE COMMENT CODENAME IS MISSING  ###############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function indicates whether measurements are above or below the LOD when the comment codename 
#          is missing but the LOD is available
#
# Inputs: df_after_harmonization - dataframe of the comment dataset with the codenames harmonized
#         df_doc_cleaning - dataframe of the dataset containing the cleaning documentation
#
# Outputs: df_clean - dataframe of the dataset where rows are the participants and columns are the harmonized
#                     codenames. Participants with measurements are indicated as above or below the LOD.

indicate_relation_to_LOD_comment_missing <- function(df_after_harmonization
                                                     , df_doc_cleaning)
{
  
  # Extract the rows where comment codename exists
  # df_doc_cleaning_chem_with_comments <- df_doc_cleaning %>%
  #   filter(!is.na(comment_codename) == TRUE)
  # # View(df_doc_cleaning_chem_with_comments)
  
  # Extract the rows where the comment codename is missing and the LOD exists
  df_doc_cleaning_missing_comment <- df_doc_cleaning %>%
    filter(#grepl("Missing comment codename|Insufficient comment indicators"
                 # , LOD_notes) == TRUE 
           #& 
             !is.na(LOD) == TRUE 
           & !is.na(comment_codename_use) == TRUE)
  # View(df_doc_cleaning_missing_comment)
  
  # Determine the chemical codename that have missing comments
  chem_missing_comment <- df_doc_cleaning_missing_comment %>%
    pull(variable_codename_use) %>%
    unique(.)
  # print(chem_missing_comment)
  
  # Determine the number of chemicals with missing comments
  num_chem_missing_comment <- length(chem_missing_comment)
  
  for(j in seq(num_chem_missing_comment))
  {
    # Extract the omment codename that has a missing comment codename
    chem_missing_j <- chem_missing_comment[j]
    
    
    # Extract the rows pertaining to the chemical with the missing comment codename
    subset_doc_miss_cleaning <- df_doc_cleaning_missing_comment %>%
      filter(variable_codename_use == chem_missing_j)
    # print(subset_doc_miss_cleaning)
    
    # Extract the missing comment codename
    comment_codename_j <- df_doc_cleaning_missing_comment %>%
      filter(variable_codename_use == chem_missing_j) %>%
      pull(comment_codename_use) %>%
      unique(.)
    # 
    
    if(is_empty(comment_codename_j) == FALSE)
    {
      # Determine the number of cycles that needs to be fixed
      num_cycles_to_fix <- nrow(subset_doc_miss_cleaning)
      # print(num_cycles_to_fix)
      
      for(k in seq(num_cycles_to_fix))
      {
        # Extract the row pertaining to a cycle that needs to include the indicators
        subset_doc_miss_cleaning_k <- subset_doc_miss_cleaning[k,]
        # print(subset_doc_miss_cleaning_k)
        
        # Determine the chemical codename pertaining to this comment codename
        chemical_codename_k <- subset_doc_miss_cleaning_k %>%
          pull(variable_codename_use) 
        # print(chemical_codename_k)
        
        # Determine the affected study year
        cycle_num_k <- subset_doc_miss_cleaning_k %>%
          pull(SDDSRVYR) 
        # print(cycle_num_k)
        
        # Determine the row indices from the comment dataset after harmonizing the codenames
        index_cycle_k_df_final <- which(df_after_harmonization$SDDSRVYR == cycle_num_k)
        # print(index_cycle_k_df_final)
        
        # Determine the LOD for the affect study year
        lod_k <- subset_doc_miss_cleaning_k %>%
          pull(LOD) 
        # print(lod_k)
        
        measurements <- df_after_harmonization[index_cycle_k_df_final
                                               , chemical_codename_k]
        # print(measurements)
        
        indicator_comments <- if_else(measurements < lod_k
                                      , 1
                                      , 0)
        # print(indicator_comments)
        
        # Determine whether the measurements was below (1) or above (0) the LOD for the affect study year
        df_after_harmonization[index_cycle_k_df_final, comment_codename_j] <- indicator_comments

        
        # # Check if the indicators are correctly assigned by comparing the measurements and the indicators
        # print(cbind(df_after_harmonization[index_cycle_k_df_final,] %>%
        #              select(all_of(comment_codename_j)
        #                     , SDDSRVYR)
        #            , measurements) %>%
        #         unique(.) %>%
        #         arrange(measurements) %>%
        #        str(.))
      }
    }
    
    subset_doc_chem_include_i <- df_doc_cleaning %>%
      filter(comment_codename_use == comment_codename_j) %>%
      filter(is.na(LOD) == FALSE)
    
    cycles_doc_include_i <- subset_doc_chem_include_i %>%
      pull(SDDSRVYR) %>%
      unique(.)
    
    
    subset_chem_include_i <- df_after_harmonization %>%
      select(all_of(comment_codename_j)
             , all_of(chem_missing_j)
             , SDDSRVYR) %>%
      drop_na(all_of(comment_codename_j))
    
    cycles_chem_include_i <- subset_chem_include_i %>%
      pull(SDDSRVYR) %>%
      unique(.)
    
    missing_cycles <- outersect(cycles_doc_include_i
                                , cycles_chem_include_i)
    
    no_missing_cycles <- is_empty(missing_cycles)
    
    if(no_missing_cycles == FALSE)
    {
      
      
      subset_doc_chem_include_i <- df_doc_cleaning %>%
        filter(variable_codename == chem_missing_j) %>%
        filter(SDDSRVYR %in% missing_cycles)
      
      is_empty_changes_in_codename <- is_empty(subset_doc_chem_include_i)
      # print(is_empty_changes_in_codename)
      
      if(is_empty_changes_in_codename == FALSE)
      {
        index_problem_cycle <- which(df_after_harmonization$SDDSRVYR %in% missing_cycles)
        
        df_after_harmonization[index_problem_cycle, comment_codename_j] <- NA
        
        subset_chem_include_i <- df_after_harmonization %>%
          select(all_of(comment_codename_j)
                 , SDDSRVYR) %>%
          drop_na(all_of(comment_codename_j))
        
        cycles_chem_include_i <- subset_chem_include_i %>%
          pull(SDDSRVYR) %>%
          unique(.)
        
        missing_cycles <- outersect(cycles_doc_include_i
                                    , cycles_chem_include_i)
        
        no_missing_cycles <- is_empty(missing_cycles)
        
        if(no_missing_cycles == FALSE)
        {
          print(chem_missing_j)
          print(comment_codename_j)
          print(cycles_doc_include_i)
          print(cycles_chem_include_i)
          print(missing_cycles)
        }
      }
    }
    
  }
  
  # Define the dataset as clean with indicators dictating whether measurements are above or below the LOD
  df_clean <- df_after_harmonization
  
  return(df_clean)
}