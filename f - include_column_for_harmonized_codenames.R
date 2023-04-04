#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###########################  FUNCTION TO INCLUDE A COLUMN FOR HARMONIZED CODENAMES ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function is to include a column with the harmonized codenames for ease of extraction.
#
# Inputs: vector_original_codenames - vector of the column names containing the original codenames 
#         vector_new_correcting_codenames - vector of the new column names containined the corrected codenames
#         df_cleaning_doc - dataframe containing the cleaning documentations
#
# Outputs: df_cleaning_doc - dataframe of the cleaning documentation with a new column of harmonized codename

include_column_for_harmonized_codenames <- function(vector_original_codenames
                                                    , vector_new_correcting_codenames
                                                    , df_cleaning_doc)
{
  # Determine the number of codename types 
  num_correcting_codenames <- length(vector_original_codenames)
  
  # For the variable codenames, make a new column to include the harmonized codenames
  for(j in seq(num_correcting_codenames))
  {
    # Extract a codename pertaining to the original codenames
    original_codename_j <- vector_original_codenames[j]
    # print(original_codename_j)
    
    # Extract a new column name 
    new_correcting_codename_j <- vector_new_correcting_codenames[j]
    # print(new_correcting_codename_j)
    
    # Make a temporary vector with the codenames of the original
    temp_codenames_to_use <- df_cleaning_doc[,original_codename_j]  %>%
      unlist(., use.names = FALSE)
    # print(unique(temp_codenames_to_use))
    
    # Define a string to contain the name of the new column showing the corrections
    corrected_codename_j <- paste("corrected"
                                  , original_codename_j
                                  , sep = "_")
    
    # Extract the vector containing the corrected codenames
    codenames_corrected_j <- df_cleaning_doc[,corrected_codename_j] %>%
      unlist(., use.names = FALSE)
    # print(unique(codenames_corrected_j))
    
    # Extract only the corrected codenames that is not missing 
    index_corrected_codename <- which(!is.na(codenames_corrected_j))
    # print(index_corrected_codename)
    
    # Ensure that the corrected codenames replace the original codenames for those that were harmonized
    # print(unique(temp_codenames_to_use[index_corrected_codename]))
    temp_codenames_to_use[index_corrected_codename] <- codenames_corrected_j[index_corrected_codename]
    # print(unique(temp_codenames_to_use[index_corrected_codename]))
    
    # Make a new column with the harmonized codenames 
    df_cleaning_doc[,new_correcting_codename_j] <- temp_codenames_to_use
  }
  
  # Deem the dataset as updated with the new column of harmonized codenames
  df_cleaning_doc_updated <- df_cleaning_doc
  
  return(df_cleaning_doc_updated)
}