record_series_of_8_as_na <- function(df_unclean
                                     , df_cleaning_doc
                                     , codename_of_variable_codenames)
{
  
  
  subset_cleaning_doc <- df_cleaning_doc %>%
    filter(is.na(convert_to_NA) == FALSE)
  # View(subset_cleaning_doc)
  
  data_type_covert_to_na <- typeof(subset_cleaning_doc$convert_to_NA)
  # print(data_type_covert_to_na)
  
    
  problematic_codenames <- subset_cleaning_doc %>%
    pull(all_of(codename_of_variable_codenames)) %>%
    unique(.) #%>%
    # .[7]
  # print(problematic_codenames)

  # problematic_codenames <- c( "PEP6F"
  #                            , "PER6F"
  #                            , "BPXML1")
  
  num_problematic_codenames <- length(problematic_codenames)
  
  for(i in seq(num_problematic_codenames))
  {
    prob_codename_i <- problematic_codenames[i]
    # print(prob_codename_i)
    
    index_codename <- which(subset_cleaning_doc[,codename_of_variable_codenames] == prob_codename_i)
    
    subset_cleaning_doc_i <- subset_cleaning_doc[index_codename,]
    # print(subset_cleaning_doc_i)
    
    data_type_probl_codename <- typeof(df_unclean[,prob_codename_i] %>% unlist(.))
    # print(data_type_probl_codename)
    
    probl_cycle <- subset_cleaning_doc_i %>%
      pull(SDDSRVYR) %>%
      sort(.)
    
    # print(probl_cycle)
    
    num_cycles <- length(probl_cycle)
    
    for(k in seq(num_cycles))
    {
      cycle_k <- probl_cycle[k]
      # print(cycle_k)
      
      subset_cleaning_doc_i_k <- subset_cleaning_doc_i %>%
        filter(SDDSRVYR == cycle_k)
      
      if(data_type_probl_codename == "integer")
      {
        # print(subset_cleaning_doc_i_k)
      }
      
      problematic_eights <- subset_cleaning_doc_i_k %>%
        pull(convert_to_NA) %>%
        unique(.)
      # print(problematic_eights)
      
      if(data_type_probl_codename == "character")
      {
        if(grepl(", ", problematic_eights) == TRUE)
        {
          value_categories_na <- problematic_eights %>%
            strsplit(., split = ", ") %>%
            unlist(.)  
        } else {
          
          value_categories_na <- as.numeric(problematic_eights)
          
        }
        
      } else if(data_type_probl_codename %in% c("double", "integer")) {
        
        if(grepl(", ", problematic_eights) == TRUE)
        {
          value_categories_na <- problematic_eights %>%
            strsplit(., split = ", ") %>%
            unlist(.)  %>%
            as.numeric(.)
        } else {
          
          value_categories_na <- as.numeric(problematic_eights)
          
        }
      }
      # print(value_categories_na)
      
      num_values_categories_na <- length(value_categories_na)
      
      
      for(j in seq(num_values_categories_na))
      {
        value_categories_na_j <- value_categories_na[j]
        # print(value_categories_na_j)

        index_value_categories_na_j <- which(df_unclean$SDDSRVYR == cycle_k &
                                               df_unclean[,prob_codename_i] == value_categories_na_j)

        # print(index_value_categories_na_j)

        # print(unique(df_unclean[index_value_categories_na_j, prob_codename_i]))
        df_unclean[index_value_categories_na_j, prob_codename_i] <- NA
        # print(unique(df_unclean[index_value_categories_na_j, prob_codename_i]))
      }
    }
  
  }
 
  df_clean <- df_unclean

  return(df_clean)
}