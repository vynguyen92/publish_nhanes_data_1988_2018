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
  
  names_dataset <- names_dataset[!(names_dataset %in% "Biomonitoring Equivalents")]
  # print(names_dataset)
  
  extract_file_columns <- function(x)
  {
    name_dataset_i <- x
    
    subset_files_i <- list_documentation[[name_dataset_i]] %>%
      select(variable_codename
             , variable_codename_use
             , file_name
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
  # print(num_unique_original_variables)
  
  harmonized_variables_dictionary <- dataset_dictionary %>%
    pull(variable_codename_use) %>%
    unique(.) 
  # print(length(harmonized_variables_dictionary))
  
  harmonized_df_files <- df_files %>%
    pull(variable_codename_use) %>%
    unique(.) 
  # print(length(harmonized_df_files))
    
  # print(outersect(harmonized_variables_dictionary
  #                 , harmonized_df_files))
  # print(num_harmonized_variables)
  
  list_stats <- list("num_unique_original_variables" = num_unique_original_variables
                     , "num_unique_harmonized_variables" = length(harmonized_df_files)
                     , "df_variables_by_cycle" = df_files)
  
  return(list_stats)
}