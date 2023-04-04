define_survey_weights_for_chem <- function(df_weights
                                           , df_chem_doc_cleaning)
{
  # Extract the harmonized chemical codenames
  chem_codenames <- df_chem_doc_cleaning %>%
    filter(variable_codename_use != weight_codename) %>%
    pull(variable_codename_use) %>%
    unique(.) 
  
  # chem_codenames <- "WT_VNSUMARS"
  
  # Determine the number of harmonized chemical codenames
  num_chem <- length(chem_codenames)
  
  for(i in seq(num_chem))
  {
    # Extract a given chemical codename
    chem_codename_i <- chem_codenames[i]
    print(chem_codename_i)
    
    # Extract the cleaning documentation for that given chemical codename
    subset_cas_chem_i <- df_chem_doc_cleaning %>%
      filter(variable_codename_use == chem_codename_i)
    # print(subset_cas_chem_i)
    
    weight_name_i <- subset_cas_chem_i %>%
      pull(variable_description_use) %>%
      unique(.)
    # print(weight_name_i)
    
    # Intialize an empty vector to eventually hold the survey weights for a given chemical codename
    empty_chem_weight <- rep(NA, nrow(df_weights))
    
    # Determine the cycles that have the measurements for the chemical codename
    unique_cycles <- subset_cas_chem_i %>%
      pull(SDDSRVYR) %>%
      unique(.)
    # print(unique_cycles)
    
    # Determine the number of cycles with available measurements for the chemical codename
    num_cycles <- length(unique_cycles)
    
    for(j in seq(num_cycles))
    {
      # Extract a given cycle for the given chemical codename
      cycle_j <- unique_cycles[j]
      print(cycle_j)
      
      # Extract the cleaning documentation for the given chemical codename and a given cycle
      subset_cas_chem_i_j <- subset_cas_chem_i %>%
        filter(SDDSRVYR == cycle_j)
      # print(subset_cas_chem_i_j)
      
      # Determine the row indices of participants who have measurements in a given cycle
      index_cycle_j <- which(df_weights$SDDSRVYR == cycle_j)
      # print(str(index_cycle_j))
      
      # Determine the codename of the survey weights used to construct the new survey weights specific for a 
      # given chemical
      weight_codename_j <- subset_cas_chem_i_j %>%
        pull(weight_codename) %>% 
        strsplit(.
                 , split = ", ") %>% 
        unlist(.) %>%  
        unique(.)
      # print(weight_codename_j)
      
      # Determine the number of survey weights codenames used in a given cycle
      # Note: some of the PFASs may have 2 survey weights to used in the same cycle, because one is for adults
      # and the other is for children
      num_weight_codenames_j <- length(weight_codename_j)
      
      for(k in seq(num_weight_codenames_j))
      {
        # Determine a given survey weight codename 
        weight_codename_j_k <- weight_codename_j[k]
        # print(weight_codename_j_k)
        
        # Determine the row indices for participants who have a survey weight for this survey weight codename
        index_existing_weight_k <- which(!is.na(df_weights[,weight_codename_j_k]))
        
        # Determine the row indices for participants who have a survey weight in this cycle and for this particular
        # survey weight codename
        index_existing_in_cycle_k <- intersect(index_existing_weight_k
                                               , index_cycle_j)
        
        # Assign the survey weights into the empty vector to construct the new survey weights specific for a 
        # chemical codename
        empty_chem_weight[index_existing_in_cycle_k] <- df_weights[index_existing_in_cycle_k
                                                                   , weight_codename_j_k]
        # View(df_weights[index_existing_in_cycle_k
        #                 , c("RIDAGEYR",weight_codename_j_k, "SDDSRVYR")] %>% 
        #        unique(.))
      }
    }
    
    # After constructing the survey weights for a given chemical codename across all the cycles with available
    # measurments, make a new column in the dataset of constructed survey weights
    df_weights <- df_weights %>%
      mutate(empty_chem_weight)
    # View(df_weights %>% select(SEQN, empty_chem_weight))
    
    # Determine the column index pertaining to this new column for the constructed survey weights
    index_new_column <- which(colnames(df_weights) == "empty_chem_weight")

    # Assign the new codename for the constructed survey weights
    colnames(df_weights)[index_new_column] <- chem_codename_i
    # print(colnames(df_weights))
    
    # Use for checking how the survey weights for the chemicals were formed
    vector_original_weights <- subset_cas_chem_i %>%
      pull(weight_codename) %>%
      unique(.) %>%
      strsplit(.
               , split = ", ") %>%
      unlist(.) %>%
      unique(.)
    # print(vector_original_weights)

    # View(df_weights %>%
    #        select(all_of(chem_codename_i)
    #               , all_of(vector_original_weights)
    #               , SDDSRVYR) %>%
    #        unique(.))
    
  }
  return(df_weights)
}