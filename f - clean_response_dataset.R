clean_response_dataset <- function(dataset_unclean
                                   , list_document_cleaning
                                   , name_dataset
                                   , dataset_clean_demo)
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
  # View(corrected_codenames)

  # corrected_codenames <- "LBXATC"
  # corrected_codenames <- "LBDBSESI1"
  # corrected_codenames <- "LBDRFOSI"

  print("Ensure all variables have only one codename")

  # For variables with multiple codenames, harmonize the codenames
  dataset_cleaner <- resolve_multiple_codenames(corrected_codenames
                                                , "corrected_variable_codename"
                                                , "variable_codename"
                                                , dataset_document_cleaning
                                                , dataset_cleaner
                                                , "SDDSRVYR")
  
  no_conversions <- dataset_document_cleaning %>%
    drop_na(unit_change) %>%
    pull(unit_change) %>%
    is_empty()
  
  if(no_conversions == FALSE)
  {
    print("Ensure units are consistent across cycles")
    
    dataset_cleaner <- ensure_consistent_units_across_cycles(dataset_cleaner
                                                             , dataset_document_cleaning)
  }
  
  print("Calculate statistics for replicates")

  dataset_cleaner <- calculate_statistics_for_replicates(dataset_cleaner
                                                         , dataset_document_cleaning
                                                         , name_dataset)


  print("Calculate estimated GFR")

  dataset_cleaner <- calculate_estimated_gfr(dataset_cleaner
                                             , dataset_clean_demo)

  print("Harmonize categories over time")

  dataset_cleaner <- harmonize_categories_over_time("Response Fix Category"
                                                    , list_document_cleaning
                                                    , dataset_cleaner)

  print("Include only harmonized and/or selected variables")

  codenames_include <- dataset_document_cleaning %>%
    pull(variable_codename_use) %>%
    unique(.)

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
                                                      , list_master_files$Response)

  return(dataset_cleaner)
}