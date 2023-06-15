create_categories_for_units_variables <- function(df_doc_cleaning
                                                   , df_unclean)
{
  all_variable_codename_use <- df_doc_cleaning %>%
    pull(variable_codename_use) %>%
    unique(.)
  
  subset_doc_cleaning <- df_doc_cleaning %>%
    filter(!is.na(create_categories))
  
  affected_codenames <- subset_doc_cleaning %>%
    pull(variable_codename_use)
  
  # print(affected_codenames)
  
  num_affected_codenames <- length(affected_codenames)
  
  for(i in seq(num_affected_codenames))
  {
    affected_unit_codename_i <- affected_codenames[i]
    # print(affected_unit_codename_i)
    
    affected_motif_i <- affected_unit_codename_i %>%
      gsub("U$", "", .)
    # print(affected_motif_i)
    
    affected_questionnaire_codename_i <- grep(affected_motif_i
                                              , all_variable_codename_use
                                              , value = TRUE) %>%
      outersect(.
                , affected_unit_codename_i)
    # print(affected_questionnaire_codename_i)
    
    num_extra_codenames <- length(affected_questionnaire_codename_i)
    
    if(num_extra_codenames > 1)
    {
      affected_questionnaire_codename_i <- affected_motif_i
    }
    # print(affected_questionnaire_codename_i)
    
    subset_doc_cleaning_i <- subset_doc_cleaning %>%
      filter(variable_codename_use == affected_unit_codename_i)
    
    affected_cycles <- subset_doc_cleaning_i %>%
      pull(SDDSRVYR)
    
    num_cycles <- length(affected_cycles)
    
    for(j in seq(num_cycles))
    {
      cycle_j <- affected_cycles[j]
      # print(cycle_j)
      
      subset_doc_cleaning_i_j <- subset_doc_cleaning_i %>%
        filter(SDDSRVYR == cycle_j)
      
      index_non_na <- which(!is.na(df_unclean[,affected_questionnaire_codename_i]))
      
      index_cycle_j <- which(df_unclean$SDDSRVYR == cycle_j)
      
      index_non_na_in_cycle_j <- intersect(index_non_na
                                           , index_cycle_j)
      # print(str(index_non_na_in_cycle_j))
      
      category_value_char <- subset_doc_cleaning_i_j %>%
        pull(create_categories) %>%
        gsub("[^0-9]", "", .) 
      
      category_value <- as.numeric(category_value_char)
      
      # print(unique(df_unclean[index_non_na_in_cycle_j, affected_unit_codename_i]))
      df_unclean[index_non_na_in_cycle_j, affected_unit_codename_i] <- category_value
      # print(unique(df_unclean[index_non_na_in_cycle_j, affected_unit_codename_i]))
      
      unit_value_in_dataset <- df_unclean[index_cycle_j, affected_unit_codename_i] %>%
        na.omit(.) %>%
        unique(.)
      
      if(unit_value_in_dataset != category_value)
      {
        print(affected_unit_codename_i)
        print(cycle_j)
        print(category_value)
      }
      
    }
  }
}