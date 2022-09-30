extract_file_columns <- function(x
                                 , list_documentations)
{
  name_dataset_i <- x
  
  subset_files_i <- list_documentations[[name_dataset_i]] %>%
    select(variable_codename_use
           , variable_description_use
           , file_name
           , file_category
           , year	
           , SDDSRVYR) %>%
    unique(.) %>%
    mutate(in_dataset = str_to_title(name_dataset_i))
  
  if(name_dataset_i == "Chemicals")
  {
    comments_i <- list_documentations[[name_dataset_i]] %>%
      select(comment_codename_use
             , variable_description_use
             , units
             , file_name
             , file_category
             , year	
             , SDDSRVYR) %>%
      drop_na(comment_codename_use) %>%
      unique(.) %>%
      rename(variable_codename_use = comment_codename_use) %>%
      mutate(in_dataset = "Comments")
    # View(comments_i)
    
    subset_files_i <- subset_files_i %>%
      full_join(.
                , comments_i
                , by = colnames(.))
    
  }
  
  return(subset_files_i)
}