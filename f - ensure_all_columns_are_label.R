#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
######################  FUNCTION TO ENSURE ALL COLUMNS ARE LABEL WITH THEIR VARIABLE NAME  ####################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function ensures that all columns have an attribute containing the variable name
#
# Inputs: dataset_merged - dataframe of the compiled dataset
#         dataset_doc_cleaning - dataframe dictating the cleaning documentation of the dataset of interest
#
# Outputs: dataset_merged - dataframe with all column labeled

ensure_all_columns_are_label <- function(dataset_merged
                                         , dataset_doc_cleaning
                                         , dataset_name = "")
{
  library("sjlabelled")
  
  # Determine codenames that do not have a label
  codenames_without_labels <- get_label(dataset_merged) %>% 
    .[. == ""] %>% 
    names(.) %>%
    unlist(., use.names = FALSE) #%>%
    # .[1:3]
  # print(codenames_without_labels)
  
  # Determine the number of codenames that do not have a label
  num_codenames_without_labels <- length(codenames_without_labels)
  # print(num_codenames_without_labels)
  
  if(num_codenames_without_labels == 0)
  {

  } else {

    # For each codename without a label, assign a label (i.e. description of the variable) to the codename
    for(k in seq(num_codenames_without_labels))
    {
      # Select a codename without a label
      codename_k <- codenames_without_labels[k]
      # print(codename_k)

      if(codename_k %in% c("SEQN"
                           , "SEQN_new"
                           , "SDDSRVYR"
                           , "survey_day"))
      {
        
        if(codename_k == "SEQN")
        {
          variable_name_k <- "Respondent sequence number"
          
        } else if(codename_k == "SEQN_new") {
        
          variable_name_k <- "Respondent sequence number that includes an identifier for NHANES III and NHANES continuous"
        
        } else if(codename_k == "SDDSRVYR") {
          
          variable_name_k <- "Release cycle number"
          # print(variable_name_k)
          
        } else if(codename_k == "survey_day") {
          
          variable_name_k <- "Day when survey was conducted"
          
        }
        
      } else {
        
        if(dataset_name == "")
        {
          # Define a subset with information on that variable
          subset_doc_cleaning_k <- dataset_doc_cleaning %>%
            filter(variable_codename_use == codename_k)
          # print(subset_doc_cleaning_k)
          
          # Determine the description of the variable
          variable_name_k <- subset_doc_cleaning_k %>%
            pull(variable_description_use) %>%
            unique(.)
          
        } else if(dataset_name == "comments") {
          
          subset_doc_cleaning_k <- dataset_doc_cleaning %>%
            filter(comment_codename_use == codename_k)
          
          # Determine the description of the variable
          variable_name_k <- subset_doc_cleaning_k %>%
            pull(variable_description_use) %>%
            unique(.) %>%
            .[1]
          
          units <- subset_doc_cleaning_k %>%
            pull(units) %>%
            unique(.) %>%
            .[1]
          
          variable_name_k <- variable_name_k %>%
            gsub(units
                 , ""
                 , .) %>%
            gsub("\\s\\(\\)"
                 , ""
                 , .) %>%
            paste("Comments code for"
                  , .
                  , sep = " ")
        }
        # print(subset_doc_cleaning_k)
       
        # print(variable_name_k)
      
      }
      
      
      if(length(variable_name_k) > 1 | length(variable_name_k) == 0)
      {
        print(codename_k)
        print(variable_name_k)
      }

      # print(codename_k)
      # print(variable_name_k)
      
      # Assign the description to that codename
      dataset_merged[,codename_k] <- set_label(dataset_merged[,codename_k]
                                               , variable_name_k)
    }
  }

  return(dataset_merged)
}