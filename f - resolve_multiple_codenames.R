#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#########################  FUNCTION TO ENSURE THAT ONE VARIABLE ONLY HAS ONE CODENAME #########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function harmonizes the codenames to ensure that a variable has only one codename.
#
# Inputs: vector_corrected_codenames - vector of the harmonized codenames
#         colname_corrected_codename - string of the column name with the harmonized codenames
#         colname_old_codename - string of the column name of the original (not harmonized) codenames
#         df_doc_cleaning - dataframe of the dataset containing the cleaning documentation
#         df_unclean - dataframe of the unclean dataset
#         colname_cycle - string of the column name pertaining to the study years
#
# Outputs: df_clean - dataframe of the dataset where rows are the participants and columns are the harmonized
#                     codenames.

resolve_multiple_codenames <- function(vector_corrected_codenames
                                       , colname_corrected_codename
                                       , colname_old_codename
                                       , df_doc_cleaning
                                       , df_unclean
                                       , colname_cycle)
{
  library("usefun")
  
  # Determine the number of codenames that needs to be harmonized
  num_corrected_codenames <- length(vector_corrected_codenames)
  # print(num_corrected_codenames)
  
  for(i in seq(num_corrected_codenames))
  {
    # Determine the corrected codename for a given variable
    corrected_codename_i <- vector_corrected_codenames[i]
    print(corrected_codename_i)
    
    # Determine the row indices that pertaining to this codename in the documentation dataset
    index_corrected_codename_i <- which(df_doc_cleaning[,colname_corrected_codename] == corrected_codename_i)
    # print(index_corrected_codename_i)
    
    # Extract the rows of the documentation dataset that only pertaining to this given codename
    subset_document_clean <- df_doc_cleaning %>%
      .[index_corrected_codename_i,] 
    
    if(colname_old_codename == "comment_codename")
    {
      subset_document_clean <- subset_document_clean %>%
        drop_na(comment_codename)
    }
    # print(subset_document_clean)
    
    # Determine the unharmonized codenames that pertained to the corrected codename
    old_codenames <- subset_document_clean[,colname_old_codename] %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    # print(old_codenames)

    # Determine the number of unharmonized codename for this one variable
    num_old_codenames <- length(old_codenames)
    # print(num_old_codenames)

    for(j in seq(num_old_codenames))
    {
      # Determine one of the unharmonized codename
      old_codename_j <- old_codenames[j]
      # print(old_codename_j)

      # Determine the row indices that pertaining to this unharmonized codename in the documentation dataset
      index_old_codename_j <- which(subset_document_clean[,colname_old_codename] == old_codename_j)
      # print(index_old_codename_j)

      # Extract the rows of the documentation dataset that only pertaining to this given unharmonized codename
      subset_document_clean_j <- subset_document_clean %>%
        .[index_old_codename_j,]
      # print(subset_document_clean_j)

      # Determine the study years that are affect by the unharmonized codename
      survey_years_j <- subset_document_clean_j$SDDSRVYR %>%
        unique(.)
      # print(survey_years_j)

      # print(df_unclean[,colname_cycle])
      # Determine the row indices of participants who are in the affected study years

      vector_cycle <- df_unclean[,colname_cycle] %>%
        as.vector(.)
      # print(vector_cycle)

      if(length(survey_years_j) == 1)
      {
        index_participants_cycle <- which(vector_cycle == survey_years_j)
      } else {
        index_participants_cycle <- which(vector_cycle %in% c(survey_years_j))
      }
      # print(df_unclean[index_participants_cycle, colname_cycle] %>% unique(.))

      
      index_not_na <- which(is.na(df_unclean[,old_codename_j]) == FALSE)
      # print(index_not_na)
      
      index_participants <- intersect(index_participants_cycle
                                      , index_not_na)
      # print(index_participants)

      # measurements <- df_unclean[index_participants, c(old_codename_j,corrected_codename_i, colname_cycle)]
      measurements <- df_unclean[index_participants, old_codename_j]
      # View(measurements)
      # print(str(measurements))

      if(old_codename_j %in% c("LB2VIE", "LBDRBFSI", "LBXRBFSI"))
      {
        df_unclean[,corrected_codename_i] <- df_unclean[, corrected_codename_i] %>%
          unlist(.) %>%
          as.double(.)
        # print(str(df_unclean[index_participants, corrected_codename_i]))
      }


      # print(str(df_unclean[index_participants,corrected_codename_i]))
      # Take the values of the participants from the vector of unharmonized codename and store it into the
      # vector of the harmonized codename
      df_unclean[index_participants, corrected_codename_i] <- measurements
      # print(str(df_unclean[index_participants,corrected_codename_i]))

      # View(df_unclean[,c(corrected_codename_i
      #                    , old_codename_j
      #                    , colname_cycle)] %>%
      #        filter(SDDSRVYR %in% survey_years_j) %>%
      #        unique(.))

    }
    
    df_unique <- df_unclean %>%
      select(all_of(corrected_codename_i)
             , all_of(old_codenames)
             , "SDDSRVYR") %>%
      unique(.)
    # View(df_unique)
    
    
    num_columns <- ncol(df_unique)
    # print(num_columns)
    
    index_na_corrected_codename <- which(is.na(df_unique[,corrected_codename_i]) == TRUE)
    
    df_unique_na_corrected <- df_unique[index_na_corrected_codename,] %>%
      mutate(num_na = rowSums(is.na(.)))
    # View(df_unique_na_corrected)
    
    df_old_variable_measurements <- df_unique_na_corrected %>%
      filter(num_na != num_columns - 1)
    
    if(colname_old_codename == "comment_codename")
    {
      subset_cleaning_old_codenames <- df_doc_cleaning %>%
        filter(comment_codename %in% old_codenames|
                 corrected_comment_codename %in% old_codenames) %>%
        filter(is.na(file_name) == FALSE) %>%
        filter(is.na(codename_change) == TRUE)
      
    } else if(colname_old_codename == "variable_codename") {
      
      subset_cleaning_old_codenames <- df_doc_cleaning %>%
        filter(variable_codename %in% old_codenames|
                 corrected_variable_codename %in% old_codenames) %>%
        filter(is.na(file_name) == FALSE) %>%
        filter(is.na(codename_change) == TRUE)
     
    }
    # print(subset_cleaning_old_codenames)
    
    
    cycles_old_variable <- unique(df_old_variable_measurements$SDDSRVYR)
    
    
    cycles_subset_cleaning <- unique(subset_cleaning_old_codenames$SDDSRVYR)
    
    
    missing_cycle <- outersect(cycles_old_variable
                               , cycles_subset_cleaning)
    # print(missing_cycle)
    
    no_missing_cycles <- is_empty(missing_cycle)
    # print(no_missing_cycles)
    
    
    if(no_missing_cycles == FALSE)
    {
      # print(subset_cleaning_old_codenames)
      if(colname_old_codename == "comment_codename")
      {
        
        subset_cleaning_old_codenames <- df_doc_cleaning %>%
          filter(comment_codename %in% old_codenames) %>%
          filter(comment_codename_use != corrected_codename_i) %>%
          filter(is.na(file_name) == FALSE)
        
      } else if(colname_old_codename == "variable_codename") {
        
        subset_cleaning_old_codenames <- df_doc_cleaning %>%
          filter(variable_codename %in% old_codenames) %>%
          filter(variable_codename_use != corrected_codename_i) %>%
          filter(is.na(file_name) == FALSE)
        
      }
      
      

      cycles_subset_cleaning <- unique(subset_cleaning_old_codenames$SDDSRVYR)

      missing_cycle <- outersect(cycles_old_variable
                                 , cycles_subset_cleaning)

      no_missing_cycles <- is_empty(missing_cycle)
      
      if(no_missing_cycles == FALSE)
      {
        print(corrected_codename_i)
        print(old_codenames)
        print(cycles_old_variable)
        print(cycles_subset_cleaning)
        print(missing_cycle)

        # print(subset_cleaning_old_codenames)

        # View(df_unique)
        # View(subset_cleaning_old_codenames)
        # View(df_old_variable_measurements)
      }
    }
    
    # # Check harmonization of codenames
    # relevant_codenames <- unique(c(corrected_codename_i
    #                                , old_codenames
    #                                , "SDDSRVYR"))
    # 
    # View(df_unclean %>%
    #         select(all_of(relevant_codenames)) %>%
    #         unique(.))
    
  }
  
  # Define the dataset as cleaned with all the codename harmonized
  df_clean <- df_unclean
  # View(df_clean)
  
  return(df_clean)
}