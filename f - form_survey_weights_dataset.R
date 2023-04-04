form_survey_weights_dataset <- function(dataset_unclean
                                        , list_document_cleaning
                                        , name_dataset = "Weights")
{
  library("tidyverse")
  
  # Extract the documentation dataset for the chemicals
  dataset_document_cleaning <- list_document_cleaning[[name_dataset]]
  
  print("Define survey weights for chemicals")
  
  dataset_cleaner <- define_survey_weights_for_chem(dataset_unclean
                                                    , dataset_document_cleaning)
  
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
  
  return(dataset_cleaner)
}