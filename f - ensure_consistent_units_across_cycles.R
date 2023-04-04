ensure_consistent_units_across_cycles <- function(df_need_harmonizing
                                                  , df_doc_cleaning)
{
  library(readr)
  
  # Extract a dataset to contain the cleaning documentation for the affect chemicals
  subset_units_changes <- df_doc_cleaning %>%
    filter(unit_change == 1)
  
  # Determine the chemical codenames that changes their units over the study years 
  codenames_units_changes <- subset_units_changes %>%
    pull(variable_codename_use) %>%
    unique(.)
  # .[1]
  # print(codenames_units_changes)
  
  # Determine number of chemicals affected by unit changes
  num_codenames_units_changes <- length(codenames_units_changes)
  
  for(i in seq(num_codenames_units_changes))
  {
    # Determine one of the affect chemical codenames
    codename_unit_changes_i <- codenames_units_changes[i]
    # print(codename_unit_changes_i)
    
    # Extract the dataset to contain the cleaning documentation for a given affected chemical
    subset_units_changes_i <- subset_units_changes %>%
      filter(variable_codename_use == codename_unit_changes_i)
    # print(subset_units_changes_i)
    
    unique_cycles <- subset_units_changes_i %>%
      pull(SDDSRVYR)
    
    num_cycles <- length(unique_cycles)
    
    for(j in seq(num_cycles))
    {
      cycle_j <- unique_cycles[j]
      # print(cycle_j)
      
      subset_units_changes_i_j <- subset_units_changes_i %>%
        filter(SDDSRVYR == cycle_j)
      # print(subset_units_changes_i_j)
      
      index_cycles_to_change <- which(df_need_harmonizing$SDDSRVYR == cycle_j)
      # print(index_cycles_to_change)
      
      measurements_to_change <-  df_need_harmonizing[index_cycles_to_change,codename_unit_changes_i]
      # print(str(measurements_to_change))
      
      seqn_to_change <- df_need_harmonizing[index_cycles_to_change,"SEQN"]
      
      converter_text <- subset_units_changes_i_j %>%
        pull(converter)
      # print(converter_text)
      
      conversion_text <- paste("measurements_to_change"
                               , converter_text
                               , sep = "")
      # print(conversion_text)
      
      converted_measurements <- eval(parse(text = conversion_text))
      # print(unique(converted_measurements/measurements_to_change))
      
      # View(cbind(converted_measurements
      #            , df_need_harmonizing[index_cycles_to_change,codename_unit_changes_i]) %>%
      #        unique(.))
      
   
      # measurements_before <- df_need_harmonizing[index_cycles_to_change,codename_unit_changes_i]
      
      df_need_harmonizing[index_cycles_to_change,codename_unit_changes_i] <- converted_measurements
      # print(str(df_need_harmonizing[index_cycles_to_change,codename_unit_changes_i]))
      # View(cbind(measurements_before
      #            , df_need_harmonizing[index_cycles_to_change,codename_unit_changes_i]) %>%
      #        unique(.))
      
      df_old_j <- data.frame(SEQN = seqn_to_change
                             , old_measurements = measurements_to_change)
      # print(str(df_old_j))
      
      if(j == 1)
      {
        df_old <- df_old_j
      } else {
        df_old <- df_old %>%
          full_join(.
                    , df_old_j
                    , by = colnames(.))
      }
    }
   
    # View(df_old)
    df_new <- df_need_harmonizing %>%
      select("SEQN"
             , all_of(codename_unit_changes_i)
             , SDDSRVYR) %>%
      filter(SDDSRVYR %in% unique_cycles) %>%
      rename("new_measurements" = all_of(codename_unit_changes_i))
    
    df_new_old <- df_new %>%
      full_join(.
                , df_old
                , by = "SEQN") %>%
      mutate(converter = new_measurements/old_measurements)

    # View(df_new_old)
    
    converter_in_dataset <- df_new_old %>%
      drop_na(converter) %>%
      pull(converter) %>%
      round(.) %>%
      unique(.) 
    
    converter_in_cleaning_doc <- subset_units_changes_i %>%
      pull(converter) %>%
      unique(.) %>%
      parse_number(.)
    
    if(converter_in_dataset != converter_in_cleaning_doc)
    {
      print(codename_unit_changes_i)
      print(converter_in_dataset)
      print(converter_in_cleaning_doc)
    }
    
  }
  
  
  
  df_clean <- df_need_harmonizing
  
  
  return(df_clean)
}