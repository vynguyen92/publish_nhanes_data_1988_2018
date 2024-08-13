create_dictionary <- function(list_dataset
                              , list_documentations)
{
  library("tidyverse")
  library("readxl")
  library("sjlabelled")
  library("stringr")
  
  # df_documentation_chemicals <- list_documentations[["Chemicals"]] %>%
  #   select("variable_codename_use"
  #          , "variable_description_use"
  #          , "units"	
  #          , "cas_num"
  #          , "comment_codename_use"	
  #          , "chemical_family"	
  #          , "chemical_family_shorten") %>%
  #   unique(.)
  # # View(df_documentation_chemicals)
  # 
  # boolean_duplicates <- duplicated(df_documentation_chemicals$variable_codename_use)
  # 
  # chemicals_duplicates <- df_documentation_chemicals %>%
  #   pull(variable_codename_use) %>%
  #   .[boolean_duplicates]
  # # print(chemicals_duplicates)
  # 
  # index_duplicates_in_documentation <- which(df_documentation_chemicals$variable_codename_use %in% chemicals_duplicates
  #                                            & is.na(df_documentation_chemicals$comment_codename_use) == TRUE)
  # 
  # df_documentation_chemicals <- df_documentation_chemicals[-index_duplicates_in_documentation,] 
  # 
  # # View(df_documentation_chemicals)
  # 
  # names_datasets <- names(list_dataset) #%>%
  # # .[1]
  # 
  # num_datasets <- length(names_datasets)
  # 
  # list_dictionary <- list()
  # 
  # for(i in seq(num_datasets))
  # {
  #   name_i <- names_datasets[i]
  #   print(name_i)
  #   
  #   dataset_i <- list_dataset[[name_i]]
  #   
  #   df_num_measurements_cycles_i <- dataset_i %>%
  #     map(.
  #         , calculate_num_measurements_cycles
  #         , df_nhanes = dataset_i)
  #   
  #   
  #   names_df_num_measurements_cycles_i <- names(df_num_measurements_cycles_i)
  #   
  #   df_num_measurements_cycles_i <- df_num_measurements_cycles_i %>%
  #     bind_rows(.) %>%
  #     mutate(variable_codename_use = names_df_num_measurements_cycles_i)
  #   # View(df_num_measurements_cycles_i)
  #   
  #   df_labels_i <- get_label(dataset_i) %>%
  #     data.frame(.)
  #   # View(df_labels_i)
  #   
  #   codenames <- rownames(df_labels_i)
  #   
  #   descriptions <- df_labels_i %>%
  #     pull(".")
  #   # print(descriptions)
  #   
  #   df_labels_i <- data.frame(variable_codename_use = codenames
  #                             , variable_description_use = descriptions
  #                             , stringsAsFactors = FALSE) %>%
  #     full_join(.
  #               , df_num_measurements_cycles_i
  #               , by = "variable_codename_use") %>%
  #     mutate(in_dataset = str_to_title(name_i))
  #   # View(df_labels_i)
  #   
  #   list_dictionary[[i]] <- df_labels_i
  # }
  
  names_dataset <- names(list_documentations) %>%
    gsub(" Fix Category$| \\(trimmed for spaces\\)$| \\(old\\)$"
         , ""
         , .) %>% 
    unique(.) 
  
  names_dataset <- names_dataset[!(names_dataset %in% c("Biomonitoring Equivalents"
                                                        , "Correspondence Table"))]
  print(names_dataset)
  
  df_original_codenames_weblinks <- names_dataset[1] %>%
    map(.
        , form_wide_original_codenames_files
        , list_documentations = list_documentations) %>%
    bind_rows(.)
  # print(unique(df_original_codenames_weblinks$file_name))
  # View(df_original_codenames_weblinks %>% filter(is.na(file_name)))
  
  # df_files <- names_dataset %>%
  #   map(.
  #       , extract_file_columns
  #       , list_documentations = list_documentations) %>%
  #   bind_rows(.)
  # # View(df_files)
  # 
  # 
  # 
  # joining_by_colnames <- function(x, y) full_join(x
  #                                                 , y
  #                                                 , by = NULL)
  # 
  # df_dictionary_merged <- list_dictionary %>%
  #   reduce(joining_by_colnames)
  # 
  # df_dictionary_merged <- df_dictionary_merged %>%
  #   left_join(.
  #             , df_documentation_chemicals
  #             , by = c("variable_codename_use"
  #                      , "variable_description_use")) %>%
  #   left_join(.
  #             , df_files %>%
  #               select(variable_codename_use
  #                      , file_category) %>%
  #               unique(.)
  #             , by = c("variable_codename_use")) %>%
  #   # mutate(file_category = tolower(file_category)) %>%
  #   relocate(file_category
  #            , .after = in_dataset)
  # 
  # 
  # index_cat_blank <- which(df_dictionary_merged$variable_codename_use %in% c("SEQN_new"
  #                                                                           , "survey_day"))
  # 
  # df_dictionary_merged[index_cat_blank,"file_category"] <- "Survey Variables"
  # 
  # df_dictionary_merged <- df_dictionary_merged %>%
  #   filter(!(variable_codename_use == "SDDSRVYR" & file_category == "Demographics"))
  # 
  # df_dictionary_merged <- df_dictionary_merged %>%
  #   relocate(any_of(c("num_participants_measurements"
  #                     , "unique_cycles"))
  #            , .after = file_category)
  # 
  # # View(df_dictionary_merged)
  # 
  # return(df_dictionary_merged)
}
