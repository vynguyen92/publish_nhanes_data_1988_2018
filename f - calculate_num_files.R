calculate_num_files <- function(list_documentation)
{
  library("tidyverse")
  
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
             , file_summary	
             , year	
             , SDDSRVYR) %>%
      unique(.) %>%
      drop_na(file_name) %>%
      mutate(in_dataset = name_dataset_i)
    
    if(name_dataset_i == "Chemicals")
    {
      comments_i <- list_documentation[[name_dataset_i]] %>%
        select(comment_codename
               , comment_codename_use
               , file_name
               , file_summary
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
      
    } else if(name_dataset_i == "Questionnaire") {
    
      subset_files_i <- subset_files_i %>%
        filter(!(file_summary %in% c("Prescription Medications - Drug Information"
                                   , "Prescription Medications")))
      
      medications_i <- list_documentation[[name_dataset_i]] %>%
        filter(file_summary %in% c("Prescription Medications - Drug Information"
                                   , "Prescription Medications")) %>%
        select(variable_codename
               , variable_codename_use
               , file_name	
               , file_summary	
               , year	
               , SDDSRVYR) %>%
        unique(.) %>%
        mutate(in_dataset = "Medications")
      
      subset_files_i <- subset_files_i %>%
        full_join(.
                  , medications_i
                  , by = colnames(.))
    }
    
    return(subset_files_i)
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
  
  df_num_files_by_modules <- df_files %>%
    select(file_name, in_dataset) %>%
    unique(.) %>%
    group_by(in_dataset) %>%
    summarise(num_files = n()) %>%
    ungroup()
  # View(df_files)
  View(df_num_files_by_modules)
  
  df_num_files_by_modules_and_study_type <- df_files %>%
    mutate(nhanes_type = ifelse(SDDSRVYR == -1
                                , "III"
                                , "Continuous")) %>%
    select(file_name
           , in_dataset
           , nhanes_type) %>%
    unique(.) %>%
    group_by(in_dataset
             , nhanes_type) %>%
    summarise(num_files = n()) %>%
    ungroup()
  View(df_num_files_by_modules_and_study_type)
  
  df_num_variable_by_module <- df_files %>%
    filter(grepl("^WT_", variable_codename) == FALSE) %>%
    filter(grepl("^VN", variable_codename) == FALSE) %>%
    drop_na(file_name) %>%
    select(variable_codename, in_dataset) %>%
    unique(.) %>%
    group_by(in_dataset) %>%
    summarise(num_variables = n()) %>%
    ungroup()
  View(df_num_variable_by_module)
  
  list_stats <- list("num_total_files" = num_unique_files
                     , "num_files_by_cycle" = df_num_files_by_cycle
                     , "num_files_by_module_and_nhanes_type" = df_num_files_by_modules_and_study_type
                     , "df_files_by_cycle" = df_files
                     , "df_num_files_by_modules" = df_num_files_by_modules)
  
  return(list_stats)
}