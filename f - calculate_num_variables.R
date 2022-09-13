calculate_num_variables <- function(list_documentation)
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
      select(variable_codename
             , variable_codename_use
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
}