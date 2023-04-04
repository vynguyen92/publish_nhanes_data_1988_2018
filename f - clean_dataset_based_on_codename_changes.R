#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
######################  FUNCTION TO CLEAN A DATASET IF THERE ARE ONLY CHANGES IN CODENAMES ####################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function cleans a dataset based on whether the same variable has different NHANES codenames, 
#          harmonizes the categories over time, and ensures that all variables have their description or 
#          name as the label.
#
# Inputs: dataset_unclean - dataframe of the compiled dataset
#         list_document_cleaning - list of datasets dictating the cleaning documentation
#         name_dataset - string of the name of the dataset to clean
#
# Outputs: dataset_cleaner - dataframe of the cleaned dataset without the extraneous codenames.

clean_dataset_based_on_codename_changes <- function(dataset_unclean
                                                    , list_document_cleaning
                                                    , name_dataset)
{
  
  # Extract the cleaning documentation for using the name of the dataset of interest
  dataset_document_cleaning <- list_document_cleaning[[name_dataset]]
  
  # Determine the codenames that needs to be corrected
  corrected_codenames <- dataset_document_cleaning %>%
    filter(is.na(corrected_variable_codename) == FALSE) %>%
    select("corrected_variable_codename") %>%
    unique(.) %>%
    unlist(., use.names = FALSE) #%>%
    # .[5]
  # print(corrected_codenames)

  print("Ensure all variables have only one codename")
  
  # For chemicals with multiple chemical codenames, harmonize the codenames
  dataset_cleaner <- resolve_multiple_codenames(corrected_codenames
                                              , "corrected_variable_codename"
                                              , "variable_codename"
                                              , dataset_document_cleaning
                                              , dataset_unclean
                                              , "SDDSRVYR")

  # Determine all the names of the datasets used for cleaning documentations
  names_document_cleaning <- names(list_document_cleaning)
  # print(names_document_cleaning)

  # Add "Fix Category" to the end of the name of the cleaning documentation dataset of interest
  name_df_fix_categories <- paste(name_dataset
                                  , "Fix Category")
  # print(name_df_fix_categories)

  print("Ensure categories are harmonized across time")
  
  # If the dataset of interest has another dataset used for harmonizing the categories and is not the occupational dataset, 
  # then harmonize the categories for consistency over the time period
  if(name_df_fix_categories %in% names_document_cleaning & name_df_fix_categories != "Occupation Fix Category")
  {

    # Ensure that categories for a varible are consistent over time
    dataset_cleaner <- harmonize_categories_over_time(name_df_fix_categories
                                                      , list_document_cleaning
                                                      , dataset_cleaner)
    
    # Extract harmonized codenames 
    variables_to_use <- dataset_document_cleaning %>%
      select(variable_codename_use) %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    
    if(name_dataset == "Demographics")
    {
      variables_to_use <- variables_to_use
      
    } else {
      variables_to_use <- c("SDDSRVYR"
                            , variables_to_use)
    }
    
    # Subset the dataset to include the participants identifiers and harmonized codenames
    dataset_cleaner <- dataset_cleaner %>%
      select("SEQN", all_of(variables_to_use))
    
    
  } else if(name_df_fix_categories == "Occupation Fix Category") {
    
    # Ensure that occupational groups are consistent over time
    dataset_cleaner <- harmonize_occupational_categories_over_time(name_df_fix_categories
                                                                   , list_document_cleaning
                                                                   , dataset_cleaner)
    
    # Add a column to list the industry-collar combinations 
    dataset_cleaner <- dataset_cleaner %>%
      mutate(VNSECTORCOLLARCURR = paste(VNINDUSTRYABBREV
                                        , VNBWCURRJOB
                                        , sep = " - ")) %>%
      mutate(VNSECTORCOLLARCURR = ifelse(grepl("NA", VNSECTORCOLLARCURR) == TRUE
                                         , NA
                                         , VNSECTORCOLLARCURR)) %>%
      # Include participants who are unemployed
      mutate(VNSECTORCOLLARCURR = case_when(is.na(VNSECTORCOLLARCURR) & OCD150 == 3 & is.na(OCQ380) == TRUE ~ "Looking for work"
                                            , is.na(VNSECTORCOLLARCURR) & OCD150 == 3 & OCQ380 == 7 ~ "Looking for work"
                                            , is.na(VNSECTORCOLLARCURR) & OCD150 == 3 & OCQ380 == 1 ~ "Taking care of house or family"
                                            , is.na(VNSECTORCOLLARCURR) & OCD150 == 3 & OCQ380 == 2 ~ "Going to school"
                                            , is.na(VNSECTORCOLLARCURR) & OCD150 == 3 & OCQ380 == 3 ~ "Retired"
                                            , is.na(VNSECTORCOLLARCURR) & OCD150 == 3 & OCQ380 == 4 ~ "Unable to work for health reasons"
                                            , is.na(VNSECTORCOLLARCURR) & OCD150 == 3 & OCQ380 == 5 ~ "On layoff"
                                            , is.na(VNSECTORCOLLARCURR) & OCD150 == 3 & OCQ380 == 6 ~ "Disabled"
                                            , is.na(VNSECTORCOLLARCURR) & OCD150 == 4 & OCQ380 == 1 ~ "Taking care of house or family"
                                            , is.na(VNSECTORCOLLARCURR) & OCD150 == 4 & OCQ380 == 2 ~ "Going to school"
                                            , is.na(VNSECTORCOLLARCURR) & OCD150 == 4 & OCQ380 == 3 ~ "Retired"
                                            , is.na(VNSECTORCOLLARCURR) & OCD150 == 4 & OCQ380 == 4 ~ "Unable to work for health reasons"
                                            , is.na(VNSECTORCOLLARCURR) & OCD150 == 4 & OCQ380 == 5 ~ "On layoff"
                                            , is.na(VNSECTORCOLLARCURR) & OCD150 == 4 & OCQ380 == 6 ~ "Disabled"
                                            , !is.na(VNSECTORCOLLARCURR) ~ VNSECTORCOLLARCURR)) %>%
      mutate(VNSECTORCOLLARCURR = ifelse(is.na(VNSECTORCOLLARCURR)
                                         , "Occupation Missing"
                                         , VNSECTORCOLLARCURR))

  } else {

    # Extract the harmonized codenames
    codename_to_use_unique <- dataset_document_cleaning %>%
      select(variable_codename_use) %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    # print(codename_to_use_unique)

    # Define a vector with the codename for the participant identifiers, the codenames of the 
    # physiological indicators, and the codename for the study year
    codename_to_use_unique <- c("SEQN"
                                , codename_to_use_unique
                                , "SDDSRVYR")
    
    if(name_dataset == "Response")
    {

      # Include two columns for race-adjusted and without race-adjusted GFRs
      dataset_cleaner <- calculate_estimated_gfr(dataset_cleaner) %>%
        # Calculate the average of the diastolic blood pressure for each participant
        mutate(VNAVEBPXDI = rowMeans(.[,c("BPXDI1"
                                          , "BPXDI2"
                                          , "BPXDI3"
                                          , "BPXDI4")]
                                     , na.rm = TRUE)) %>%
        # Calculate the average of the diastolic blood pressure for each participant
        mutate( VNAVEBPXSY = rowMeans(.[,c("BPXSY1"
                                           , "BPXSY2"
                                           , "BPXSY3"
                                           , "BPXSY4")]
                                      , na.rm = TRUE)) 

    }
    
    dataset_cleaner <- dataset_cleaner %>%
      # Select the columns that are harmonized
      select(all_of(codename_to_use_unique))

  }

  print("Ensure all columns have a label")
  
  # Ensure that all columns have the name of the variable as the label
  dataset_cleaner <- ensure_all_columns_are_label(dataset_cleaner
                                                  , dataset_document_cleaning)

  # Ensure that the study year is numeric as a data type
  dataset_cleaner$SDDSRVYR <- as.numeric(dataset_cleaner$SDDSRVYR)

  # Assign the name of the codename for the study year
  attr(dataset_cleaner$SDDSRVYR, "label") <- "Data release cycle"

  # Ensure that the participant ID is an integer
  dataset_cleaner$SEQN <- as.integer(dataset_cleaner$SEQN)

  # Assign the name of the codename for the participant ID
  attr(dataset_cleaner$SEQN, "label") <- "Respondent sequence number"
  
  return(dataset_cleaner)
}