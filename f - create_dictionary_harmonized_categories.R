create_dictionary_harmonized_categories <- function(list_documentations)
{
  names_doc_cleaning <- c("Demographics Fix Category"
                          , "Questionnaire Fix Category"
                          , "Response Fix Category"
                          , "Dietary Fix Category")
  
  num_datasets_harmonized_cats <- length(names_doc_cleaning)
  
  list_harmonized_categories <- list()
  
  for(i in seq(num_datasets_harmonized_cats))
  {
    name_doc_i <- names_doc_cleaning[i]
    
    in_dataset_i <- gsub(" Fix Category"
                         , ""
                         , name_doc_i)
    
    dataset_variable_descript_i <- list_documentations[[in_dataset_i]] %>%
      select(variable_codename_use
             , variable_description_use) %>%
      unique(.)
    
    if(in_dataset_i == "Dietary")
    {
      dataset_doc_i <- list_documentations[[name_doc_i]] %>%
        select(new_categories	
               , new_categories_description
               , corrected_codename_2day_separate
               , new_codename
               ) #%>%
        # mutate(new_codename = corrected_codename_2day_separate)
      # View(dataset_doc_i)
    } else {
      dataset_doc_i <- list_documentations[[name_doc_i]] %>%
        select(new_categories	
               , new_categories_description
               , new_codename)
    }
    
    dataset_doc_i <- dataset_doc_i %>%
      unique(.) %>%
      mutate(in_dataset = in_dataset_i) %>%
      rename(variable_codename_use = new_codename) %>%
      rename(harmonized_categories = new_categories) %>%
      mutate(harmonized_categories = as.character(harmonized_categories)) %>%
      rename(harmonized_categories_description = new_categories_description) %>%
      left_join(.
                , dataset_variable_descript_i
                , by = "variable_codename_use") 
    
    if(in_dataset_i == "Dietary")
    {
      dataset_doc_i <- dataset_doc_i %>%
        select(variable_codename_use
               , variable_description_use
               , harmonized_categories
               , harmonized_categories_description
               , in_dataset
               , corrected_codename_2day_separate) 
    
    } else {
      dataset_doc_i <- dataset_doc_i %>%
        select(variable_codename_use
               , variable_description_use
               , harmonized_categories
               , harmonized_categories_description
               , in_dataset)
    }
    
    
    
    
    

    # print(str(dataset_doc_i))
    
    list_harmonized_categories[[name_doc_i]] <- dataset_doc_i
  }
  
  joining_by_colnames <- function(x, y) full_join(x
                                                  , y
                                                  , by = NULL)
  
  df_dictionary_harmonized_cats <- list_harmonized_categories %>%
    reduce(joining_by_colnames)
  
  return(df_dictionary_harmonized_cats)
  
}