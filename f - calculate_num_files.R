calculate_num_files <- function(list_documentation)
{
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
      select(variable_codename_use
             , file_name	
             , file_summary	
             , year	
             , SDDSRVYR) %>%
      unique(.) %>%
      drop_na(file_name) %>%
      mutate(in_dataset = name_dataset_i)
  }
  
  df_files <- names_dataset %>%
    map(.
        , extract_file_columns) %>%
    bind_rows(.)
  # View(df_files)
  
  num_unique_files <- df_files %>%
    pull(file_name) %>%
    unique(.) %>%
    length(.)
  # print(num_unique_files)
  
  df_num_files_by_cycle <- df_files %>%
    select(file_name, SDDSRVYR) %>%
    unique(.) %>%
    group_by(SDDSRVYR) %>%
    summarise(num_files = n()) %>%
    ungroup()
  # print(df_num_files_by_cycle)
  
  list_stats <- list("num_total_files" = num_unique_files
                     , "num_files_by_cycle" = df_num_files_by_cycle
                     , "df_files_by_cycle" = df_files)
  
  return(list_stats)
}