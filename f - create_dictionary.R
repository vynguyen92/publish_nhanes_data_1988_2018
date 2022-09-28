create_dictionary <- function(list_dataset
                              , list_documentations)
{
  library("tidyverse")
  library("readxl")
  library("sjlabelled")
  
  df_documentation_chemicals <- list_documentations[["Chemicals"]] %>%
    select("variable_codename_use"
           , "variable_description_use"
           , "units"	
           , "cas_num"
           , "comment_codename_use"	
           , "chemical_family"	
           , "chemical_family_shorten") %>%
    unique(.)
  # View(df_documentation_chemicals)
  
  boolean_duplicates <- duplicated(df_documentation_chemicals$variable_codename_use)
  
  chemicals_duplicates <- df_documentation_chemicals %>%
    pull(variable_codename_use) %>%
    .[boolean_duplicates]
  # print(chemicals_duplicates)
  
  index_duplicates_in_documentation <- which(df_documentation_chemicals$variable_codename_use %in% chemicals_duplicates
                                             & is.na(df_documentation_chemicals$comment_codename_use) == TRUE)
  
  df_documentation_chemicals <- df_documentation_chemicals[-index_duplicates_in_documentation,] 
  
  # View(df_documentation_chemicals)
  
  names_datasets <- names(list_dataset) #%>%
  # .[1]
  
  num_datasets <- length(names_datasets)
  
  list_dictionary <- list()
  
  for(i in seq(num_datasets))
  {
    name_i <- names_datasets[i]
    print(name_i)
    
    dataset_i <- list_dataset[[name_i]]
    
    df_labels_i <- get_label(dataset_i) %>%
      data.frame(.)
    # View(df_labels_i)
    
    codenames <- rownames(df_labels_i)
    
    descriptions <- df_labels_i %>%
      pull(".")
    # print(descriptions)
    
    df_labels_i <- data.frame(variable_codename_use = codenames
                              , variable_description_use = descriptions
                              , stringsAsFactors = FALSE) %>%
      mutate(in_dataset = name_i)
    # View(df_labels_i)
    
    list_dictionary[[i]] <- df_labels_i
  }
  
  joining_by_colnames <- function(x, y) full_join(x
                                                  , y
                                                  , by = NULL)
  
  df_dictionary_merged <- list_dictionary %>%
    reduce(joining_by_colnames)
  
  df_dictionary_merged <- df_dictionary_merged %>%
    left_join(.
              , df_documentation_chemicals
              , by = c("variable_codename_use"
                       , "variable_description_use"))
  
  return(df_dictionary_merged)
}
