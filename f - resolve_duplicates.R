resolve_duplicates <- function(dataset_unclean)
{
 
  library(tidyverse)
  library(usefun)
   # dataset_unclean <- dataset_unclean[!duplicated(as.list(dataset_unclean))]
  
  # dataset_unclean <- dataset_unclean[, unique(colnames(dataset_unclean))]
  
  # Determine any codenames that ends with .x or .y, as this indicates that these
  # variables are duplicates
  remaining_codenames <- colnames(dataset_unclean) %>%
    .[grepl("\\.[a-z]$", .) == TRUE] %>%
    sort(.)
  # print(remaining_codenames)
  
  # Replace the .x or .y with a blank to get the original codename
  problematic_codenames <- unique(gsub(".(x|y)", "", remaining_codenames))
  # print(problematic_codenames)
  
  # Determine number of duplicates
  num_problematic_codenames <- length(problematic_codenames)
  # print(num_problematic_codenames)
 
  if(num_problematic_codenames != 0)
  {
    for(p in seq(num_problematic_codenames))
    {
      # Start the pth duplicated biomarker
      probl_codename <- problematic_codenames[p]
      # print(probl_codename)
      
      probl_codename_pattern <- paste("\\b"
                                      , probl_codename
                                      , "\\b"
                                      , sep = "")
      
      problematic_duplicated_codenames <- remaining_codenames %>%
        .[grepl(probl_codename_pattern, .) == TRUE]
      # print(problematic_duplicated_codenames)
      
      
      # Determine the number of duplicates for this pth biomarker
      num_probl_dupl <- length(problematic_duplicated_codenames)
      # print(num_probl_dupl)
      
      # Determine column with lowest number of values
      df_num_values <- dataset_unclean %>%
        select("SEQN"
               , all_of(problematic_duplicated_codenames)) %>%
        pivot_longer(cols = all_of(problematic_duplicated_codenames)
                     , names_to = "codenames"
                     , values_to = "values") %>%
        group_by(codenames) %>%
        summarise(count = sum(!is.na(values))) %>%
        ungroup(.)
      # print(df_num_values)
      
      # Determine codename with lowest number of values to hold the other values in other columns
      first_probl_code_dupl <- df_num_values %>%
        filter(count == min(.$count)) %>%
        pull(codenames) %>%
        .[1]
      # print(first_probl_code_dupl)
      
      # Determine the remaining codenames that do NOT have the lowest number of values
      remaining_probl_code_dupl <- outersect(first_probl_code_dupl
                                             , problematic_duplicated_codenames)
      # print(remaining_probl_code_dupl)
      
      num_remaining_probl_code_dupl <- length(remaining_probl_code_dupl)
      # print(num_remaining_probl_code_dupl)
      
      # For any subsequent duplicate (kth), extract all measurements and store it to the corresponding
      # participant in the first duplicated vector
      for(k in num_remaining_probl_code_dupl)
      {
        # Determine the index pertaining the kth duplicated column vector for the pth biomarker
        dupl_codename_k <- remaining_probl_code_dupl[k]
        # print(dupl_codename_k)
        
        # Store the column vector for this kth duplicate into a more readable variable name
        measurements_dupl_k <- dataset_unclean[,dupl_codename_k] %>%
          data.frame(.)
        # print(str(measurements_dupl_k))
        
        # Determine the indices of participants who have measurements for this pth biomarker in the
        # kth duplicated column vector
        index_measurements_for_dupl_k <- which(!is.na(measurements_dupl_k))
        # print(index_measurements_for_dupl_k)
        
        # print(str( measurements_dupl_k[index_measurements_for_dupl_k,]))
        
        # View(dataset_unclean[,c(first_probl_code_dupl, dupl_codename_k)] %>% unique(.))
        
        # Extract the measurements and store it into the column vector pertaining to the first duplicate
        dataset_unclean[index_measurements_for_dupl_k,first_probl_code_dupl] <-
          measurements_dupl_k[index_measurements_for_dupl_k,]
        
        # View(dataset_unclean[,c(first_probl_code_dupl, dupl_codename_k)] %>% unique(.))
      }
      # print(dataset_unclean[,problematic_duplicated_codenames] %>%
      #        unique(.))
      
      # Determine the index of the column containing all measurements of the duplicates
      index_first_probl <- which(colnames(dataset_unclean) == first_probl_code_dupl)
      # print(index_first_probl)
      
      # print(colnames(dataset_unclean)[index_first_probl])
      # Change the column name to the harmonized codename
      colnames(dataset_unclean)[index_first_probl] <- probl_codename
      # print(colnames(dataset_unclean)[index_first_probl])
      
      # Determine the index of the columns that are the remaining duplicates
      index_remove_dupl <- which(colnames(dataset_unclean) %in% remaining_probl_code_dupl)
      
      # Remove the duplicates from the dataset
      dataset_unclean <- dataset_unclean[,-index_remove_dupl]
      
      # print(colnames(dataset_unclean)[grepl(probl_codename_pattern, colnames(dataset_unclean)) == TRUE])
    }
    
    dataset_clean <- dataset_unclean[,unique(colnames(dataset_unclean))]
  } else {
    dataset_clean <- dataset_unclean
  }

  return(dataset_clean)
}