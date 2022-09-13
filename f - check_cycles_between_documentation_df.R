check_cycles_between_documentation_df <- function(dataset_unclean
                                                  , dataset_doc_cleaning
                                                  , dataset_name = "")
{

  dataset_doc_cleaning <- dataset_doc_cleaning %>%
    filter(is.na(problem_with_file) == TRUE)
  
  if(dataset_name == "")
  {
    variables_included <- dataset_doc_cleaning %>%
      pull(variable_codename_use) %>%
      unique(.)
    
  } else if(dataset_name == "comments") {
    
    variables_included <- dataset_doc_cleaning %>%
      drop_na(comment_codename_use) %>%
      pull(comment_codename_use) %>%
      unique(.)
  }
  
  num_chemicals <- length(variables_included)
  
  for(i in seq(num_chemicals))
  {
    variable_i <- variables_included[i]
    # print(variable_i)
 
    
    if(dataset_name == "")
    {
      subset_include_i <- dataset_doc_cleaning %>%
        filter(variable_codename_use == variable_i)
      
    } else if(dataset_name == "comments") {
      
      subset_include_i <- dataset_doc_cleaning %>%
        filter(comment_codename_use == variable_i)
    }
    
    cycles_doc_i <- subset_include_i %>%
      pull(SDDSRVYR) %>%
      unique(.)
    
    
    subset_nhanes_i <- dataset_unclean %>%
      select(all_of(variable_i)
             , SDDSRVYR) %>%
      na.omit(.)
    
    cycles_nhanes_i <- subset_nhanes_i %>%
      pull(SDDSRVYR) %>%
      unique(.)
    
    missing_cycles <- outersect(cycles_doc_i
                                , cycles_nhanes_i)

    
    no_missing_cycles <- is_empty(missing_cycles)
    
    if(no_missing_cycles == FALSE)
    {
      # print(variable_i)
      # print(cycles_doc_i)
      # print(cycles_nhanes_i)
      # print(missing_cycles)
      
      if(dataset_name == "")
      {
        subset_doc_original_i <- dataset_doc_cleaning %>%
          filter(variable_codename == variable_i) %>%
          filter(SDDSRVYR %in% missing_cycles)
        
      } else if(dataset_name == "comments") {
        
        subset_doc_original_i <- dataset_doc_cleaning %>%
          filter(comment_codename == variable_i) %>%
          filter(SDDSRVYR %in% missing_cycles)
      }
      
      
      # print(subset_doc_original_i)
      
      if("codename_change" %in% colnames(subset_doc_original_i))
      {
        codename_change_i <- subset_doc_original_i %>%
          pull(codename_change) %>%
          is_empty(.)
        # print(codename_change_i)
        
        if(codename_change_i == FALSE)
        {
          index_cycles_missing <- which(dataset_unclean[,"SDDSRVYR"] %in% missing_cycles)
          
          dataset_unclean[index_cycles_missing, variable_i] <- NA
        }
        
        subset_nhanes_i <- dataset_unclean %>%
          select(all_of(variable_i)
                 , SDDSRVYR) %>%
          na.omit(.)
        
        cycles_nhanes_i <- subset_nhanes_i %>%
          pull(SDDSRVYR) %>%
          unique(.)
        
        missing_cycles <- outersect(cycles_doc_i
                                    , cycles_nhanes_i)
        
        no_missing_cycles <- is_empty(missing_cycles)
        
        if(no_missing_cycles == FALSE)
        {
          print(variable_i)
          print(cycles_doc_i)
          print(cycles_nhanes_i)
          print(missing_cycles)
        }
        
      } else {
        print(variable_i)
        print(cycles_doc_i)
        print(cycles_nhanes_i)
        print(missing_cycles)
      }
    }
  }
  
  dataset_clean <- dataset_unclean
  
  return(dataset_clean)
}