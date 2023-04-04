clean_chemicals_dataset <- function(dataset_unclean
                                    , list_document_cleaning
                                    , name_dataset
                                    )
{
  library(tidyverse)
  
  # Extract the cleaning documentation for using the name of the dataset of interest
  dataset_document_cleaning <- list_document_cleaning[[name_dataset]] %>%
    filter(is.na(problem_with_file) == TRUE) 
  # View(dataset_document_cleaning)
  
  print("Ensure series of 8 are recorded as NA")
  
  dataset_cleaner <- record_series_of_8_as_na(dataset_unclean
                                              , dataset_document_cleaning
                                              , "variable_codename")
  
  
  # Determine the codenames that needs to be corrected
  corrected_codenames <- dataset_document_cleaning %>%
    filter(is.na(statistic_replicates ) == TRUE) %>%
    filter(is.na(corrected_variable_codename) == FALSE) %>%
    pull("corrected_variable_codename") %>%
    unique(.) #%>%
  # .[1]
  # print(corrected_codenames)
  
  print("Ensure all variables have only one codename")
  
  # For chemicals with multiple chemical codenames, harmonize the codenames
  dataset_cleaner <- resolve_multiple_codenames(corrected_codenames
                                                , "corrected_variable_codename"
                                                , "variable_codename"
                                                , dataset_document_cleaning
                                                , dataset_cleaner
                                                , "SDDSRVYR")
  
  print("Ensure units are consistent across cycles")

  dataset_cleaner <- ensure_consistent_units_across_cycles(dataset_cleaner
                                                           , dataset_document_cleaning)

  print("Calculate statistics for replicates")

  dataset_cleaner <- calculate_statistics_for_replicates(dataset_cleaner
                                                         , dataset_document_cleaning
                                                         , name_dataset)

  # print("Exclude measurements due to drastic changes in LOD")
  # 
  # dataset_cleaner <- exclude_measurements_based_changes_in_lod(dataset_cleaner
  #                                                              , dataset_document_cleaning)

  dataset_harmonized_and_unharmonized <- dataset_cleaner

  print("Include only harmonized and/or selected variables")

  codenames_include <- dataset_document_cleaning %>%
    drop_na(variable_codename_use) %>%
    pull(variable_codename_use) %>%
    unique(.)
  # print(codenames_include)

  dataset_cleaner <- dataset_cleaner[,c("SEQN"
                                        , "SEQN_new"
                                        , "SDDSRVYR"
                                        , codenames_include)] %>%
    drop_na(SEQN)

  print("Check number of cycles matches between dataset and documentation")
  
  dataset_cleaner <- check_cycles_between_documentation_df(dataset_cleaner
                                                           , dataset_document_cleaning)

  print("Ensure all columns are label")

  dataset_cleaner <- ensure_all_columns_are_label(dataset_cleaner
                                                  , dataset_document_cleaning)

  list_chemicals <- list("only_harmonized_variable" = dataset_cleaner
                         , "harmonized_and_unharmonized_variables" = dataset_harmonized_and_unharmonized)

  return(list_chemicals)
}