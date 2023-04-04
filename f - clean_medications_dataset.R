clean_medications_dataset <- function(dataset_unclean
                                        , list_document_cleaning
                                        , name_dataset)
{
  library(tidyverse)
  
  # Extract the cleaning documentation for using the name of the dataset of interest
  dataset_document_cleaning <- list_document_cleaning[[name_dataset]] %>%
    filter(grepl("\\bPrescription Medications\\b", .$file_summary) == TRUE &
             is.na(.$SDDSRVYR) == FALSE)
  
  # Determine the codenames that needs to be corrected
  corrected_codenames <- dataset_document_cleaning %>%
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
                                                , dataset_unclean
                                                , "SDDSRVYR")
  
  
  print("Ensure series of 8 are recorded as NA")
  
  dataset_cleaner <- record_series_of_8_as_na(dataset_cleaner
                                              , dataset_document_cleaning
                                              , "variable_codename_use")
  
  print("Insert descriptions of drugs")
  
  dataset_cleaner <- insert_descriptions_drugs(dataset_cleaner)
  
  print("Ensure factor columns are character")
  
  dataset_cleaner <- convert_from_factor_to_char(dataset_cleaner)
  
  print("Include only harmonized and/or selected variables")

  codenames_include <- dataset_document_cleaning %>%
    filter(variable_codename_use != "SEQN") %>%
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

  print("Ensure all columns are labelled")

  dataset_cleaner <- ensure_all_columns_are_label(dataset_cleaner
                                                  , dataset_document_cleaning)
  
  return(dataset_cleaner)
}