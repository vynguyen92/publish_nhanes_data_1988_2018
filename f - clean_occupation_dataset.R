clean_occupation_dataset <- function(dataset_unclean
                                     , list_document_cleaning)
{
  # Extract the cleaning documentation for using the name of the dataset of interest
  dataset_document_cleaning <- list_document_cleaning[["Occupation"]] 
  
  # Determine the codenames that needs to be corrected
  corrected_codenames <- dataset_document_cleaning %>%
    filter(is.na(corrected_variable_codename) == FALSE) %>%
    pull("corrected_variable_codename") %>%
    unique(.) #%>%
  
  print("Ensure all variables have only one codename")
  
  # For chemicals with multiple chemical codenames, harmonize the codenames
  dataset_cleaner <- resolve_multiple_codenames(corrected_codenames
                                                , "corrected_variable_codename"
                                                , "variable_codename"
                                                , dataset_document_cleaning
                                                , dataset_unclean
                                                , "SDDSRVYR")
  
  print("Harmonize occupational categories over time")
  
  dataset_cleaner <- harmonize_occupational_categories_over_time("Occupation Fix Category"
                                                                 , list_document_cleaning
                                                                 , dataset_cleaner)
  
  print("Define sector-collar combinations")
  
  dataset_cleaner <- dataset_cleaner %>%
    mutate(VNSECTORCOLLARCURR =  paste(VNINDUSTRYABBREV
                                       , VNBWCURRJOB
                                       , sep = " - ")) %>%
    mutate(VNSECTORCOLLARCURR = ifelse(grepl("NA", VNSECTORCOLLARCURR) == TRUE
                                       , NA
                                       , VNSECTORCOLLARCURR))
  
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