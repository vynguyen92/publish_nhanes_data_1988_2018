#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#################  FUNCTION TO FORM A DATASET TO INDICATE MEASUREMENTS ABOVE OR BELOW THE LOD #################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function forms a dataset containing indicators to dicate whether measurements are above or below
#          the LOD.
#
# Inputs: dataset_unclean - dataframe of the compiled dataset
#         list_document_cleaning - list of datasets dictating the cleaning documentation
#         name_dataset - string of the name of the dataset to clean
#
# Outputs: dataset_clean - dataframe of the comments dataset where rows are the participants and columns are the
#                          comment codenames.

form_comments_dataset <- function(dataset_unclean
                                  , list_document_cleaning
                                  , name_dataset = "Chemicals")
{
  library("usefun")
  library("tidyverse")
  
  # Extract the documentation dataset for the chemicals
  dataset_document_cleaning <- list_document_cleaning[[name_dataset]]
  
  # Extract the codenames of the chemical comments 
  comment_codenames <- dataset_document_cleaning %>%
    drop_na(comment_codename) %>%
    pull(comment_codename) %>%
    unique(.) 
  # print(comment_codenames)
  
  # outersect(comment_codenames, colnames(dataset_unclean)) %>%
  #   print(.)
  
  # View(data.frame(colnames(dataset_unclean)))
  
  # dataset_cleaner <- dataset_unclean %>%
  #   select("SEQN"
  #          , "SEQN_new"
  #          , all_of(comment_codenames)
  #          , "SDDSRVYR")

  print("Ensure a chemical comment has only one codename")
  
  # Determine the codenames with the correction
  corrected_codenames <- dataset_document_cleaning %>%
    filter(is.na(corrected_comment_codename) == FALSE &
             is.na(comment_codename) == FALSE) %>%
    pull(corrected_comment_codename) %>%
    unique(.)
  # print(corrected_codenames)
  # print(length(corrected_codenames))
  
  # For chemicals with multiple chemical codenames, harmonize the codenames
  dataset_cleaner <- resolve_multiple_codenames(corrected_codenames
                                                , "corrected_comment_codename"
                                                , "comment_codename"
                                                , dataset_document_cleaning
                                                , dataset_unclean
                                                , "SDDSRVYR")

  print("Include indicators for participants being above or below the LOD for cycles with missing comment codename")

  # For study years where there is not a comment codename but an LOD, indicate whether participants are above or below
  # the LOD
  dataset_cleaner <- indicate_relation_to_LOD_comment_missing(dataset_cleaner
                                                            , dataset_document_cleaning)


  # print("Exclude indicators when participants do not have measurements")
  # 
  # dataset_cleaner <- exclude_extraneous_indicators(dataset_cleaner
  #                                                  , dataset_document_cleaning)

  print("Include reasonable comments (i.e. values are 0, 1, or 2)")

  dataset_cleaner <- ensure_reasonable_comments(dataset_cleaner
                                                , dataset_document_cleaning)

  print("Include only harmonized and/or selected variables")

  # Define a vector of the harmonized comment codenames
  included_codenames_unique <- dataset_document_cleaning %>%
    drop_na(comment_codename_use) %>%
    drop_na(LOD) %>%
    pull(comment_codename_use) %>%
    unique(.)

  dataset_cleaner <- dataset_cleaner %>%
    select("SEQN"
           , "SEQN_new"
           , all_of(included_codenames_unique)
           , "SDDSRVYR")

  print("Check number of cycles matches between dataset and documentation")

  dataset_cleaner <- check_cycles_between_documentation_df(dataset_cleaner
                                                           , dataset_document_cleaning
                                                           , "comments")

  print("Ensure all columns are label")

  dataset_cleaner <- ensure_all_columns_are_label(dataset_cleaner
                                                  , dataset_document_cleaning
                                                  , "comments")

  # print(colnames(dataset_cleaner))

  return(dataset_cleaner)
}