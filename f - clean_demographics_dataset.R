clean_demographics_dataset <- function(dataset_unclean
                                       , list_document_cleaning
                                       , name_dataset)
{
  library(tidyverse)
  
  # Extract the cleaning documentation for using the name of the dataset of interest
  dataset_document_cleaning <- list_document_cleaning[[name_dataset]]
  
  dataset_cleaner <- dataset_unclean %>%
    mutate(age_group = case_when(HSAGEIR <= 16 ~ "YOUTH"
                                 , HSAGEIR > 16 ~ "ADULTS"
                                 , RIDAGEYR <= 19 ~ "YOUTH"
                                 , RIDAGEYR > 19 ~ "ADULTS"))
  # View(dataset_cleaner %>%
  #        select(HSAGEIR
  #               , RIDAGEYR
  #               , age_group) %>% 
  #        unique(.))
  
  print("Ensure series of 8 are recorded as NA")
  
  dataset_cleaner <- record_series_of_8_as_na(dataset_cleaner
                                              , dataset_document_cleaning
                                              , "variable_codename")
  
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
                                                , dataset_cleaner
                                                , "SDDSRVYR")
  
  
  print("Harmonize categories of variables over time")

  name_df_fix_categories <- paste(name_dataset
                                  , "Fix Category"
                                  , sep = " ")

  dataset_cleaner <- harmonize_categories_over_time(name_df_fix_categories
                                                    , list_document_cleaning
                                                    , dataset_cleaner)

  print("Include only harmonized and/or selected variables")

  codenames_include <- dataset_document_cleaning %>%
    pull(variable_codename_use) %>%
    unique(.)

  dataset_cleaner <- dataset_cleaner[,c("SEQN"
                                        , "SEQN_new"
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