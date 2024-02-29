calculate_num_corrections <- function(list_documentation)
{
  library(usefun)
  library(tidyverse)
  
  # print(names(list_documentation))
  
  names_dataset <- names(list_documentation) %>%
    gsub(" Fix Category$| \\(trimmed for spaces\\)$| \\(old\\)$"
         , ""
         , .) %>% 
    unique(.) 
  
  names_dataset <- names_dataset[!(names_dataset %in% c("Biomonitoring Equivalents"
                                                        , "Correspondence Table"))]
  
  # print(names_dataset)
  
  names_fix_cats <- names(list_documentation) %>%
    .[grepl("Fix Category$",.) == TRUE]
  # print(names_fix_cats)
  
  list_main_documentation <- list_documentation[names_dataset]
  print(names(list_main_documentation))
  
  list_fix_cats_documentation <- list_documentation[names_fix_cats]
  print(names(list_fix_cats_documentation))
  
  for(i in seq(length(list_main_documentation)))
  {
    # print(names_dataset[i])
    # print(str(list_main_documentation[[i]]$convert_to_NA))
    
    data_type <- typeof(list_main_documentation[[i]]$convert_to_NA)
    # print(data_type)
    
    length_convert <- length(list_main_documentation[[i]]$convert_to_NA)
    
    if(length_convert != 0 & data_type == "double")
    {
      list_main_documentation[[i]]$convert_to_NA <- as.character(list_main_documentation[[i]]$convert_to_NA)
    }
    
    list_main_documentation[[i]] <- list_main_documentation[[i]] %>%
      mutate(in_dataset = names_dataset[i])
  }
  
 
  
  df_main_documentation <- list_main_documentation %>%
    reduce(.
           , full_join
           , by = NULL)
  print(colnames(df_main_documentation))
  # View(df_main_documentation)
  
  # print(df_main_documentation %>% pull(variable_codename_use) %>% unique(.) %>% length(.))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Changes in codenames ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  df_codename_changes <- df_main_documentation %>%
    filter(codename_change == 1) %>%
    filter(grepl("replicate", variable_description_use) == FALSE)
  # View(df_codename_changes)
  
  df_comment_changes <- df_main_documentation %>%
    filter(is.na(comment_codename) == FALSE) %>%
    filter(comment_codename != corrected_comment_codename) %>%
    filter(grepl("replicate", variable_description_use) == FALSE)
  # View(df_comment_changes)
  
  num_variables_codename_changes <- df_codename_changes %>%
    pull(variable_codename) %>%
    unique(.) %>%
    length(.)
  
  num_variables_codename_changes_harmonized <- df_codename_changes %>%
    pull(variable_codename_use) %>%
    unique(.) %>%
    length(.)
  
  num_comment_changes <- df_comment_changes %>%
    pull(comment_codename)%>%
    unique(.) %>%
    length(.)
  # print(num_variables_codename_changes)
  
  num_comment_changes_harmonized <- df_comment_changes %>%
    pull(comment_codename_use)%>%
    unique(.) %>%
    length(.)
  
  total_codenames_changes <- num_variables_codename_changes + num_comment_changes
  # print(total_codenames_changes)
  
  total_codenames_changes_harmonized <- num_variables_codename_changes_harmonized + num_comment_changes_harmonized
  
  df_stats_codename_changes_by_module <- df_codename_changes %>%
    select(variable_codename, in_dataset) %>%
    unique(.) %>%
    group_by(in_dataset) %>%
    summarise(num_unharmonized = n()) %>%
    ungroup(.) %>%
    add_row(in_dataset = "Comments"
            , num_unharmonized = num_comment_changes) %>%
    add_row(in_dataset = "All"
            , num_unharmonized = total_codenames_changes) %>%
    mutate(correction_type = "codename changes") 
  
  df_stats_codename_changes_by_module_harmonized <- df_codename_changes %>%
    select(variable_codename_use, in_dataset) %>%
    unique(.) %>%
    group_by(in_dataset) %>%
    summarise(num_harmonized = n()) %>%
    ungroup(.) %>%
    add_row(in_dataset = "Comments"
            , num_harmonized = num_comment_changes_harmonized) %>%
    add_row(in_dataset = "All"
            , num_harmonized = total_codenames_changes_harmonized) %>%
    mutate(correction_type = "codename changes") 
    
  df_stats_codenames_changes <- df_stats_codename_changes_by_module %>%
    full_join(.
              , df_stats_codename_changes_by_module_harmonized
              , by = NULL)
  # View(df_stats_codenames_changes)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~ Changes in codenames for questionnaires ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  df_codename_changes_questionnaire <- df_main_documentation %>%
    filter(grepl("Derive", codename_note))
  # print(df_codename_changes_questionnaire)
  
  num_harmonized <- df_codename_changes_questionnaire %>%
    pull(variable_codename_use) %>%
    unique(.) %>%
    length(.)
  # print(num_harmonized)
  
  num_unharmonized <- num_harmonized*2
  # print(num_unharmonized)
  
  # print(df_stats_codenames_changes)
  
  df_codename_changes_questionnaire_consolidate <- df_main_documentation %>%
    filter(grepl("Consolidate into one column", codename_note))
  # print(df_codename_changes_questionnaire_consolidate)
  
  num_consolidate_age_at_first_diagnosis_cancer <- nrow(df_codename_changes_questionnaire_consolidate)
  
  num_age_at_first_diagnosis_cancer <- nrow(list_documentation[["Correspondence Table"]])
  
  index_question_changes_codename <- which(df_stats_codenames_changes$in_dataset == "Questionnaire"
                                           & df_stats_codenames_changes$correction_type == "codename changes")
  
  df_stats_codenames_changes[index_question_changes_codename,"num_harmonized"] <- df_stats_codenames_changes[index_question_changes_codename,"num_harmonized"]+
    num_harmonized + 
    num_age_at_first_diagnosis_cancer
  
  df_stats_codenames_changes[index_question_changes_codename,"num_unharmonized"] <- df_stats_codenames_changes[index_question_changes_codename,"num_unharmonized"]+
    num_unharmonized + 
    num_consolidate_age_at_first_diagnosis_cancer + 
    num_age_at_first_diagnosis_cancer
  
  # View(df_stats_codenames_changes)
  
  # View(df_codename_changes_questionnaire)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~ Changes in codenames for replicates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  df_codename_changes_replicates <- df_main_documentation %>%
    filter(codename_change == 1) %>%
    filter(grepl("replicate", variable_description_use) == TRUE)
  
  df_stats_codename_changes_replicates <- df_codename_changes_replicates %>%
    select(variable_codename, in_dataset) %>%
    unique(.) %>%
    group_by(in_dataset) %>%
    summarise(num_unharmonized = n()) %>%
    ungroup(.) %>%
    mutate(correction_type = "codename changes for replicates")
  print(df_stats_codename_changes_replicates)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~ Changes in units for the same variable ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  df_changes_in_units <- df_main_documentation %>%
    filter(unit_change == 1)
  # print(df_changes_in_units)
  
  num_variables_unit_changes <- df_changes_in_units %>%
    pull(variable_codename_use) %>%
    unique(.) %>%
    length(.)
  # print(num_variables_unit_changes)
  
  df_stats_unit_changes <- data.frame(in_dataset = "Chemicals"
                                      , num = num_variables_unit_changes
                                      , correction_type = "changes in units") %>%
    add_row(in_dataset = "All"
            , num = num_variables_unit_changes
            , correction_type = "changes in units")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~ Convert 8-fills to NA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  df_8_fills_to_na <- df_main_documentation %>%
    filter(is.na(convert_to_NA) == FALSE)
  
  num_variables_8_fills <- df_8_fills_to_na %>%
    pull(variable_codename) %>%
    unique(.) %>%
    length(.)
  # print(num_variables_8_fills)
  
  df_stats_8_fills_to_na <- df_8_fills_to_na %>%
    select(variable_codename, in_dataset) %>%
    # unique(.) %>%
    group_by(in_dataset) %>%
    summarise(num = n()) %>%
    ungroup(.) %>%
    add_row(in_dataset = "All"
            , num = num_variables_8_fills) %>%
    mutate(correction_type = "convert 8-fills to NA")
  print(df_stats_8_fills_to_na)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Summarize multiple replicates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  df_replicates <- df_main_documentation %>%
    filter(grepl("replicate", variable_description_use) == TRUE) %>%
    filter(in_dataset != "Weights")
  # View(df_replicates)
  
  df_replicates_harmonized <- df_main_documentation %>%
    filter(statistic_replicates == "rowMeans")
  # View(df_replicates_harmonized)
    
  num_replicates  <- df_replicates %>%
    pull(variable_codename) %>%
    unique(.) %>%
    length(.)
  # print(num_replicates)
  
  num_summarized_replicated <- df_replicates_harmonized %>%
    pull(variable_codename_use) %>%
    unique(.) %>%
    length(.)
  # print(num_summarized_replicated)
  
  df_stats_replicates_unharmonized <- df_replicates %>%
    select(variable_codename, in_dataset) %>%
    unique(.) %>%
    group_by(in_dataset) %>%
    summarise(num_unharmonized = n()) %>%
    ungroup(.) %>%
    add_row(in_dataset = "All"
            , num_unharmonized = num_replicates) %>%
    mutate(correction_type = "summarize replicates")
  # print(df_stats_replicates_unharmonized)
  
  df_stats_replicates_harmonized <- df_replicates_harmonized %>%
    select(variable_codename_use, in_dataset) %>%
    unique(.) %>%
    group_by(in_dataset) %>%
    summarise(num_harmonized = n()) %>%
    ungroup(.) %>%
    add_row(in_dataset = "All"
            , num_harmonized = num_summarized_replicated) 
  # print(df_stats_replicates_harmonized)
  
  df_stats_replicates <- df_stats_replicates_unharmonized %>%
    full_join(.
              , df_stats_replicates_harmonized
              , by = "in_dataset")
  # print(df_stats_replicates)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Correction with comments ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  df_comment <- df_main_documentation %>%
    # filter(grepl("replicate", variable_description_use) == TRUE) %>%
    filter(in_dataset == "Chemicals")
  View(df_comment)
  
  num_lod_missing_entries <- df_comment %>%
    filter(grepl("Missing", LOD_notes) == TRUE) %>%
    nrow(.)
  
  num_variables_lod_missing <- df_comment %>%
    filter(grepl("Missing", LOD_notes) == TRUE) %>%
    filter(is.na(comment_codename_use) == FALSE) %>%
    pull(comment_codename_use) %>%
    unique(.) %>%
    length(.)
  
  print(num_variables_lod_missing)
  
  # num_LODs_documented
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Changes in Categories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  for(j in seq(length(list_fix_cats_documentation)))
  {
    # print(names_fix_cats[j])
    
    data_type_cat_num <- typeof(list_fix_cats_documentation[[j]]$categories_num)
    
    length_convert_cat_num <- length(list_fix_cats_documentation[[j]]$categories_num)
    
    if(length_convert_cat_num != 0 & data_type_cat_num == "double")
    {
      list_fix_cats_documentation[[j]]$categories_num <- as.character(list_fix_cats_documentation[[j]]$categories_num)
    }
    
    data_type_new_cat <- typeof(list_fix_cats_documentation[[j]]$new_categories)
    
    length_convert_new_cat <- length(list_fix_cats_documentation[[j]]$new_categories)
    
    if(length_convert_new_cat != 0 & data_type_new_cat == "double")
    {
      list_fix_cats_documentation[[j]]$new_categories <- as.character(list_fix_cats_documentation[[j]]$new_categories)
    }
    
    list_fix_cats_documentation[[j]] <- list_fix_cats_documentation[[j]] %>%
      mutate(in_dataset = names_fix_cats[j])
  }
  
  df_fix_cats_documentation <- list_fix_cats_documentation %>%
    reduce(.
           , full_join
           , by = NULL)
  # View(df_fix_cats_documentation)
  # print(colnames(df_fix_cats_documentation))
  
  subset_fixed_cats <- df_fix_cats_documentation %>%
    filter(categories_num != new_categories)
  # View(subset_fixed_cats)
  
  num_fixed_cats <- nrow(subset_fixed_cats)
  
  num_fixed_cats_variable <- subset_fixed_cats %>%
    pull(new_codename) %>%
    unique(.) %>%
    length(.)
  # print(num_fixed_cats_variable)
  
  df_fixed_cats <- subset_fixed_cats %>%
    select(categories_num, in_dataset) %>%
    group_by(in_dataset) %>%
    summarise(num = n()) %>%
    ungroup(.) %>%
    add_row(in_dataset = "All"
            , num = num_fixed_cats) %>%
    mutate(correction_type = "changes in categories - count categories")
  # print(df_fixed_cats)
  
  df_fixed_cats_variables <- subset_fixed_cats %>%
    select(new_codename, in_dataset) %>%
    unique(.) %>%
    group_by(in_dataset) %>%
    summarise(num = n()) %>%
    ungroup(.) %>%
    add_row(in_dataset = "All"
            , num = num_fixed_cats_variable) %>%
    mutate(correction_type = "changes in categories - count variables")
  # print(df_fixed_cats_variables)
  
  list_stats <- list(df_stats_codenames_changes
                     , df_stats_unit_changes
                     , df_stats_8_fills_to_na
                     , df_stats_replicates
                     , df_fixed_cats
                     , df_fixed_cats_variables)
  
  df_stats <- list_stats %>%
    reduce(full_join
           , by = NULL) 
  View(df_stats)
  
}
