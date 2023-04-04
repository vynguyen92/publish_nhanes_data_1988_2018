#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################################  FUNCTION TO CLEAN THE NHANES MORTALITY DATASET ##############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function cleans the mortality dataset to have non-NA columns and adds columns for variables 
#          where the categories are labeled instead of numerataed
#
# Inputs: dataset_unclean - dataframe of the compiled mortality dataset
#         list_document_cleaning - list of data frames for cleaning documentation
#         name_dataset - string for the name of the cleaning documentation for the mortality dataset
#
# Outputs: dataset_clean_updated - dataframe of the cleaned mortality dataset

clean_mortality_dataset <- function(dataset_unclean
                                    , list_document_cleaning
                                    , name_dataset)
{
  library(tidyverse)
  library(sjlabelled)
  
  dataset_doc_cleaning <- list_document_cleaning[[name_dataset]]
  
  # Determine the number of columns in the dataset
  num_cols <- ncol(dataset_unclean)
  
  # Initiate an empty vector to hold the indices to remove empty columns
  indices_to_remove <- c()
  
  print("Exclude variables with no data")
  
  # Perform for each variable
  for(i in seq(num_cols))
  {
    # Determine whether the column is empty
    is_empty <- all(is.na(dataset_unclean[,i]))
    
    # If the column is empty, then indicate that this variable will be removed
    if(is_empty == TRUE)
    {
      indices_to_remove <- append(indices_to_remove
                                  , i)
    }
    
    # Ensure the class of this variable 
    class_i <- class(dataset_unclean[,i])
    class(dataset_unclean[,i]) <- c(class_i)
    # print(class(dataset_unclean[,i]))
    
  }
  
  # Define a dataframe with the empty columns removed 
  dataset_unclean_updated <- dataset_unclean[,-indices_to_remove] %>%
    unlabel(.)
  
  # Define the name of the dataset that has the categories labeled for each categorical variable
  name_fix_categories <- paste(name_dataset
                               , "Fix Category")

  # Extract the dataset with the categories labeled for each categorical variable
  df_doc_revise_categories <- list_document_cleaning[[name_fix_categories]]

  # Determine the codenames for the categorical variables 
  new_codenames <- df_doc_revise_categories %>%
    select(new_codename) %>%
    unique(.) %>%
    unlist(., use.names = FALSE)
  
  # Determine the number of categorical variables 
  num_new_codenames <- length(new_codenames)

  print("Add in columns with categories labeled")
  
  for(j in seq(num_new_codenames))
  {
    # Determine the codename of one of the categorical variables
    new_codename_j <- new_codenames[j]
    # print(new_codename_j)

    # Extract the cleaning documentation pertaining to this particular codenames
    subset_doc_revise_categories <- df_doc_revise_categories %>%
      filter(new_codename == new_codename_j)
    # print(subset_doc_revise_categories)

    # Determine the codename of the particular categorical variable
    original_codename <- subset_doc_revise_categories %>%
      select(codename_original) %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    # print(original_codename)

    # Extract the numbers of the categories and the descriptions
    subset_categories <- subset_doc_revise_categories %>%
      select("categories_num", "categories_description")

    # Manually assign the first column (numbers) with the codename of the particular categorical variable
    colnames(subset_categories)[1] <- original_codename
    
    # Manually assign the second column (descriptions) with the new codename of the particular categorical variable
    colnames(subset_categories)[2] <- new_codename_j

    # Merge the description into the NHANES dataset
    dataset_unclean_updated <- dataset_unclean_updated %>%
      left_join(.
                , subset_categories
                , by = original_codename)

    # Extract the name of the new variable
    variable_description_j <- subset_doc_revise_categories %>%
      select(variable_description) %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    # print(variable_description_j)
   
    # Assign the new variable with the name
    attr(dataset_unclean_updated[,new_codename_j], "label") <- variable_description_j

  }
  
  print("Check number of cycles matches between dataset and documentation")
  
  dataset_doc_cleaning_to_check <- dataset_doc_cleaning %>%
    filter(is.na(codename_note) == TRUE)
  
  dataset_unclean_updated <- check_cycles_between_documentation_df(dataset_unclean_updated
                                                                   , dataset_doc_cleaning_to_check)
  
  print("Ensure all columns have a label")
  
  # Ensure all variables have their attribute label as the name of the variable
  dataset_clean_updated <- ensure_all_columns_are_label(dataset_unclean_updated
                                                          , dataset_doc_cleaning)
  
  # Ensure that the participant ID is an integer
  dataset_clean_updated$SEQN <- as.integer(dataset_clean_updated$SEQN)
  
  # Assign the name of the codename for the participant ID
  attr(dataset_clean_updated$SEQN, "label") <- "Respondent sequence number"
  
  attr(dataset_clean_updated$SEQN_new, "label") <- "Respondent sequence number that includes an identifier for NHANES III and NHANES continuous"
  
  attr(dataset_clean_updated$SDDSRVYR, "label") <- "Release cycle number"
  # print(attr(dataset_clean_updated$SDDSRVYR, "label"))
  
  dataset_clean_updated <- dataset_clean_updated %>%
    relocate(SEQN_new, .after = SEQN)
  
  return(dataset_clean_updated)
}
