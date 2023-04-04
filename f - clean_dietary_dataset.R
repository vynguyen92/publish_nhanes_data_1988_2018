clean_dietary_dataset <- function(dataset_unclean
                                  , list_document_cleaning
                                  , name_dataset)
{
  library(tidyverse)
  
  # Extract the cleaning documentation for using the name of the dataset of interest
  dataset_document_cleaning <- list_document_cleaning[[name_dataset]] 
  
  print("Ensure series of 8 are recorded as NA")
  
  dataset_cleaner <- record_series_of_8_as_na(dataset_unclean
                                              , dataset_document_cleaning
                                              , "variable_codename_use")
  
  print("Calculate sum for fatty acids")

  dataset_cleaner <- calculate_statistics_for_replicates(dataset_cleaner
                                                         , dataset_document_cleaning
                                                         , "Dietary")

  print("Change SEQN_new from C-1e+05 to C-100000")

  index_problem_seqn_new <- which(dataset_cleaner$SEQN_new == "C-1e+05")

  dataset_cleaner[index_problem_seqn_new,"SEQN_new"] <- "C-100000"

  print("Ensure factor columns are character")
  
  dataset_cleaner <- convert_from_factor_to_char(dataset_cleaner)
  
  print("Check number of cycles matches between dataset and documentation")
  
  dataset_cleaner <- check_cycles_between_documentation_df(dataset_cleaner
                                                           , dataset_document_cleaning)
  
  print("Ensure all columns are label")

  dataset_cleaner <- ensure_all_columns_are_label(dataset_cleaner
                                                  , list_master_files$Dietary)
  
  return(dataset_cleaner)
}