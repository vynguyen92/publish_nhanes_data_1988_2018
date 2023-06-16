consolidate_two_variables_into_one <- function(df_doc_cleaning
                                               , df_unclean)
{
  subset_variables_derived <- df_doc_cleaning %>%
    filter(grepl("Derive", codename_note))
  View(subset_variables_derived)
  
  codenames_derived <- subset_variables_derived %>%
    pull(variable_codename_use) %>%
    unique(.)
  print(codenames_derived)
  
  # num_codenames_derived <- length(codenames_derived)
  # # print(num_codenames_derived)
  # 
  # for(i in seq(num_codenames_derived))
  # {
  #   corrected_codename_i <- codenames_derived[i]
  #   
  #   subset_derived_i <- subset_variables_derived %>%
  #     filter(variable_codename_use == corrected_codename_i)
  #   # print(subset_derived_i)
  #   
  #   unique_cycles <- subset_derived_i %>%
  #     pull(SDDSRVYR)
  #   # print(unique_cycles)
  #   
  #   index_affected_cycles <- which(df_unclean$SDDSRVYR %in% unique_cycles)
  #   # print(unique(df_unclean[index_affected_cycles, "SDDSRVYR"]))
  #   
  #   codename_note_i <- subset_derived_i %>%
  #     pull(codename_note) %>%
  #     unique(.)
  #   # print(codename_note_i)
  #   
  #   codenames_used_for_deriving <- codename_note_i %>%
  #     gsub("Derive from ", "", .) %>%
  #     strsplit(., split = " and ") %>%
  #     unlist(.)
  #   # print(codenames_used_for_deriving)
  #   
  #   questionaire_codename <- codenames_used_for_deriving %>%
  #     grepl("G$", .) %>%
  #     codenames_used_for_deriving[.]
  #   # print(questionaire_codename)
  #   
  #   value_codename <- codenames_used_for_deriving %>%
  #     grepl("Q$", .) %>%
  #     codenames_used_for_deriving[.]
  #   # print(value_codename)
  #   
  #   questionaire_variable <- df_unclean[index_affected_cycles, questionaire_codename]
  #   
  #   value_variable <- df_unclean[index_affected_cycles, value_codename]
  #   
  #   # print(unique(df_unclean[index_affected_cycles,corrected_codename_i]))
  #   df_unclean[index_affected_cycles,corrected_codename_i] <- questionaire_variable
  #   # print(unique(df_unclean[index_affected_cycles,corrected_codename_i]))
  #   
  #   
  #   
  #   index_questionaire_variable_is_1 <- which(df_unclean[,corrected_codename_i] == 1 & df_unclean$SDDSRVYR %in% unique_cycles)
  #   # print(unique(df_unclean[index_questionaire_variable_is_1,corrected_codename_i]))
  #   
  #   df_unclean[index_questionaire_variable_is_1,corrected_codename_i] <- df_unclean[index_questionaire_variable_is_1,value_codename]
  #   
  #   # # For checking
  #   # View(df_unclean[index_affected_cycles,c(corrected_codename_i
  #   #                                         , questionaire_codename
  #   #                                         , value_codename
  #   #                                         , "SDDSRVYR")] %>%
  #   #        unique(.))
  # }
  # 
  # df_clean <- df_unclean
  # 
  # return(df_clean)
}