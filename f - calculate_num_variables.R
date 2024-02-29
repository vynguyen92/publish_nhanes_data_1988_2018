calculate_num_variables <- function(list_documentation
                                    , dataset_dictionary)
{
  library(usefun)
  library(tidyverse)
  
  names_dataset <- names(list_documentation) %>%
    gsub(" Fix Category$| \\(trimmed for spaces\\)$| \\(old\\)$"
         , ""
         , .) %>% 
    unique(.) 
  
  names_dataset <- names_dataset[!(names_dataset %in% c("Biomonitoring Equivalents"
                                                        , "Correspondence Table"))]
  # print(names_dataset)
  
  extract_file_columns <- function(x)
  {
    name_dataset_i <- x
    
    subset_files_i <- list_documentation[[name_dataset_i]] %>%
      select(variable_codename
             , variable_codename_use
             , file_name
             , file_category
             , year	
             , SDDSRVYR) %>%
      unique(.) %>%
      mutate(in_dataset = name_dataset_i)
    
    if(name_dataset_i == "Chemicals")
    {
      comments_i <- list_documentation[[name_dataset_i]] %>%
        select(comment_codename
               , comment_codename_use
               , file_name
               , file_category
               , year	
               , SDDSRVYR) %>%
        drop_na(comment_codename_use) %>%
        unique(.) %>%
        rename(variable_codename = comment_codename) %>%
        rename(variable_codename_use = comment_codename_use) %>%
        mutate(in_dataset = "Comments")
      
      subset_files_i <- subset_files_i %>%
        full_join(.
                  , comments_i
                  , by = colnames(.))
        
    }
    
    return(subset_files_i)
  }
  
  df_files <- names_dataset %>%
    map(.
        , extract_file_columns) %>%
    bind_rows(.)
  # View(df_files)
  
  num_unique_original_variables <- df_files %>%
    filter(grepl("^WT_", variable_codename) == FALSE) %>%
    filter(grepl("^VN", variable_codename) == FALSE) %>%
    drop_na(file_name) %>%
    pull(variable_codename) %>%
    unique(.) %>%
    length(.)
  # View(num_unique_original_variables)
  # print(num_unique_original_variables)
  
  harmonized_variables_dictionary <- dataset_dictionary %>%
    pull(variable_codename_use) %>%
    unique(.) 
  # print(length(harmonized_variables_dictionary))
  
  harmonized_df_files <- df_files %>%
    pull(variable_codename_use) %>%
    unique(.) 
  # print(length(harmonized_df_files))
  
  # View(df_files)
    
  df_num_variables_per_module <- dataset_dictionary %>%
    select(variable_codename_use
           , in_dataset) %>%
    unique() %>%
    group_by( in_dataset) %>%
    count() %>%
    ungroup()
  # View(df_num_variables_per_module)
  
  
  df_num_variables_per_module_file_cat <- df_files %>%
    select(variable_codename_use
           , file_category
           , in_dataset) %>%
    unique() %>%
    group_by(file_category
             , in_dataset) %>%
    count() %>%
    ungroup()
  # View(df_num_variables_per_module)
  
  # print(outersect(harmonized_variables_dictionary
  #                 , harmonized_df_files))
  # print(num_harmonized_variables)
  
  list_stats <- list("num_unique_original_variables" = num_unique_original_variables
                     , "num_unique_harmonized_variables" = length(harmonized_variables_dictionary)
                     , "df_variables_by_cycle" = df_files
                     , "df_num_variables_per_module" = df_num_variables_per_module
                     , "df_num_variables_per_module_file_cat" = df_num_variables_per_module_file_cat)
  
  return(list_stats)
}