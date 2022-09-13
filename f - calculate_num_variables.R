calculate_num_variables <- function(list_documentation
                                    , dataset_dictionary)
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
             , file_name
             , year	
             , SDDSRVYR) %>%
      unique(.) %>%
      mutate(in_dataset = name_dataset_i)
  }
  
  df_files <- names_dataset %>%
    map(.
        , extract_file_columns) %>%
    bind_rows(.)
  View(df_files)
  
  num_unique_original_variables <- df_files %>%
    filter(grepl("^WT_", variable_codename) == FALSE) %>%
    filter(grepl("^VN", variable_codename) == FALSE) %>%
    drop_na(file_name) %>%
    pull(variable_codename) %>%
    unique(.) %>%
    length(.)
  print(num_unique_original_variables)
  
  num_harmonized_variables <- dataset_dictionary %>%
    pull(variable_codename_use) %>%
    unique(.) %>%
    length(.)
  print(num_harmonized_variables)
}