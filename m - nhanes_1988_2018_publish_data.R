#############################################################################################################
################################  MAIN SCRIPT - FORMING NHANES MERGED DATASET  ##############################
#############################################################################################################

working_directory <- "/Users/vynguyen/Dropbox/Mac/Documents/GitHub/publish_nhanes_data_1988_2018"

setwd(working_directory)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Assign Directories of NHANES Datasets  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

mortality_directory <- paste(working_directory
                             , "Mortality Datasets"
                             , sep = "/") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Compile the NHANES Datasets  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Extract the individual mortality datasets and compile them into the unclean mortality dataset
mortality_unclean <- compile_mortality_dataset(dataset_directory = mortality_directory
                                               , current_directory = working_directory)

setwd(working_directory)
response_unclean <- compile_datasets(cleaning_documentation = list_master_files$Response
                                     , current_directory = working_directory
                                     , name_dataset = "Response")

dietary_unclean <- NHANES_dietary_final


demographics_unclean <- compile_datasets(cleaning_documentation = list_master_files$Demographics
                                         , current_directory = working_directory
                                         , name_dataset = "Demographics")


# Fix this due to change from df to list for the cleaning documentation
medications_unclean <- compile_datasets(cleaning_documentation = list_master_files$Questionnaire %>%
                                          filter(grepl("\\bPrescription Medications\\b"
                                                       , .$file_summary) == TRUE &
                                                   is.na(.$SDDSRVYR) == FALSE)
                                        , current_directory = working_directory
                                        , name_dataset = "Medications")


chemicals_unclean <- compile_datasets(cleaning_documentation = list_master_files$Chemicals
                                      , current_directory = working_directory
                                      , name_dataset = "Chemicals")


weights_unclean <- compile_datasets(cleaning_documentation = list_master_files$Weights
                                    , current_directory = working_directory
                                    , name_dataset = "Weights")

occupations_unclean <- compile_datasets(cleaning_documentation = list_master_files$Occupation
                                        , current_directory = working_directory
                                        , name_dataset = "Occupation")


setwd(working_directory)
questionnaire_unclean <- compile_datasets(cleaning_documentation = list_master_files$Questionnaire %>%
                                                 filter(grepl("\\bPrescription Medications\\b"
                                                              , .$file_summary) == FALSE &
                                                          is.na(.$SDDSRVYR) == FALSE)
                                          , current_directory = working_directory
                                          , name_dataset = "Questionnaire")

extract_series_of_8(dataset_unclean = questionnaire_unclean
                    , name_dataset = "questionnaire")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Clean the NHANES Datasets  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Clean the mortality dataset
mortality_clean <- clean_mortality_dataset(mortality_unclean
                                           , list_master_files
                                           , "Mortality")

dietary_clean <- clean_dietary_dataset(dietary_unclean
                                       , list_master_files
                                       , "Dietary")

demographics_clean <- clean_demographics_dataset(demographics_unclean
                                                 , list_master_files
                                                 , "Demographics")

setwd(working_directory)
response_clean <- clean_response_dataset(response_unclean
                                         , list_master_files
                                         , "Response"
                                         , demographics_clean)

medications_clean <- clean_medications_dataset(medications_unclean
                                               , list_master_files
                                               , "Questionnaire")

list_chemicals_clean <- clean_chemicals_dataset(chemicals_unclean
                                                , list_master_files
                                                , "Chemicals")

chemicals_clean <- list_chemicals_clean$only_harmonized_variable

comments_unclean <- list_chemicals_clean$harmonized_and_unharmonized_variables

rm(list_chemicals_clean)

comments_clean <- form_comments_dataset(comments_unclean
                                        , list_master_files
                                        , "Chemicals")

# Upload the cleaning documentation for all datasets
list_master_files <- upload_nhanes_master_files("NHANES - Master List of Files 1i.xlsx")

weights_clean <- form_survey_weights_dataset(weights_unclean
                                             , list_master_files
                                             , "Weights")

occupation_clean <- clean_occupation_dataset(occupations_unclean
                                             , list_master_files)

questionnaire_clean <- clean_questionnaire_dataset(questionnaire_unclean
                                                   , list_master_files
                                                   , "Questionnaire")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Create dictionary  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

df_medications_drug_info <- create_dictionary_drugs("RXQ_DRUG")

df_dictionary <- create_dictionary(list_dataset = list("mortality" = mortality_clean
                                                       , "dietary" = dietary_clean
                                                       , "demographics" = demographics_clean
                                                       , "response" = response_clean
                                                       , "medications" = medications_clean
                                                       , "questionnaire" = questionnaire_clean
                                                       , "chemicals" = chemicals_clean
                                                       , "occupation" = occupation_clean
                                                       , "weights" = weights_clean
                                                       , "comments" = comments_clean)
                                   , list_documentations = list_master_files)

df_levels_categorical_variables <- create_dictionary_harmonized_categories(list_documentations = list_master_files)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Calculate statistics on the dataset  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

list_num_files <- calculate_num_files(list_master_files)

list_num_variables <- calculate_num_variables(list_master_files
                                              , df_dictionary)

list_num_corrections <- calculate_num_corrections(list_master_files)
