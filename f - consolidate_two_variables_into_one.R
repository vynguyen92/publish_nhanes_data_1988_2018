consolidate_two_variables_into_one <- function(df_doc_cleaning
                                               , name_fix_categories_of_df
                                               , list_cleaning_documentation
                                               , df_unclean)
{
  subset_variables_derived <- df_doc_cleaning %>%
    filter(grepl("Derive", codename_note))
  # View(subset_variables_derived)
  
  codenames_derived <- subset_variables_derived %>%
    pull(variable_codename_use) %>%
    unique(.)
  # print(codenames_derived)
  
  dataset_fix_categories <- list_cleaning_documentation[[name_fix_categories_of_df]]
  
  num_codenames_derived <- length(codenames_derived)
  # print(num_codenames_derived)

  for(i in seq(num_codenames_derived))
  {
    corrected_codename_i <- codenames_derived[i]
    # print(corrected_codename_i)

    subset_derived_i <- subset_variables_derived %>%
      filter(variable_codename_use == corrected_codename_i)
    # print(subset_derived_i)

    unique_cycles <- subset_derived_i %>%
      pull(SDDSRVYR)
    # print(unique_cycles)

    index_affected_cycles <- which(df_unclean$SDDSRVYR %in% unique_cycles)
    # print(unique(df_unclean[index_affected_cycles, "SDDSRVYR"]))

    codename_note_i <- subset_derived_i %>%
      pull(codename_note) %>%
      unique(.)
    # print(codename_note_i)

    codenames_used_for_deriving <- codename_note_i %>%
      gsub("Derive from ", "", .) %>%
      strsplit(., split = " and ") %>%
      unlist(.)
    # print(codenames_used_for_deriving)

    questionaire_codename <- codenames_used_for_deriving %>%
      grepl("G$", .) %>%
      codenames_used_for_deriving[.]
    # print(questionaire_codename)

    value_codename <- codenames_used_for_deriving %>%
      grepl("Q$", .) %>%
      codenames_used_for_deriving[.]
    # print(value_codename)

    questionaire_variable <- df_unclean[index_affected_cycles, questionaire_codename]

    value_variable <- df_unclean[index_affected_cycles, value_codename]

    # print(unique(df_unclean[index_affected_cycles,corrected_codename_i]))
    df_unclean[index_affected_cycles,corrected_codename_i] <- questionaire_variable
    # print(unique(df_unclean[index_affected_cycles,corrected_codename_i]))



    index_questionaire_variable_is_1 <- which(df_unclean[,corrected_codename_i] == 1 & df_unclean$SDDSRVYR %in% unique_cycles)
    # print(unique(df_unclean[index_questionaire_variable_is_1,corrected_codename_i]))

    df_unclean[index_questionaire_variable_is_1,corrected_codename_i] <- df_unclean[index_questionaire_variable_is_1,value_codename]

    # print(questionaire_codename)
    subset_fix_categories <- dataset_fix_categories %>%
      filter(new_codename == corrected_codename_i) %>%
      filter(codename_original == questionaire_codename)
    # print(subset_fix_categories)
    
    num_problematic_categories <- nrow(subset_fix_categories)
    
    for(j in seq(num_problematic_categories))
    {
      subset_fix_categories_j <- subset_fix_categories[j,]
      # print(subset_fix_categories_j)
      
      problematic_category_j <- subset_fix_categories_j %>%
        pull(categories_num)
      
      corrected_category_j <- subset_fix_categories_j %>%
        pull(new_categories) %>%
        as.numeric(.)
      
      index_problematic_category_j <- which(df_unclean[,questionaire_codename] == problematic_category_j)
      df_unclean[index_problematic_category_j,corrected_codename_i] <- corrected_category_j
    }
    
    # For checking
    df_checking <- df_unclean[index_affected_cycles,c(corrected_codename_i
                                                      , questionaire_codename
                                                      , value_codename
                                                      , "SDDSRVYR")] %>%
      unique(.) %>%
      drop_na(all_of(corrected_codename_i))
    # View(df_checking)
    
    df_harmonized_values <- df_checking %>%
      filter(is.na(!!sym(value_codename)) == TRUE) %>%
      select(-SDDSRVYR) %>%
      unique(.) %>%
      select(where(~!all(is.na(.)))) %>%
      arrange(!!sym(corrected_codename_i)) %>%
      mutate_all(as.numeric)
    
    
    df_documented_values <- subset_fix_categories %>%
      select(new_categories
             , categories_num) 
    colnames(df_documented_values) <- c(corrected_codename_i
                                        , questionaire_codename)
    
    df_documented_values <- df_documented_values %>%
      arrange(!!sym(corrected_codename_i)) %>%
      mutate_all(as.numeric) %>%
      as.data.frame(.)
    
    
    are_categories_the_same <- identical(df_harmonized_values
                                         , df_documented_values)
    # print(are_categories_the_same)
    
    if(are_categories_the_same == FALSE)
    {
      print(corrected_codename_i)
      print(df_checking)
      print(df_harmonized_values)
      print(df_documented_values)
    }
    
  }

  df_clean <- df_unclean

  return(df_clean)
}