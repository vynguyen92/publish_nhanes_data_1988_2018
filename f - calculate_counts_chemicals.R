calculate_counts_chemicals <- function(dataset_chemicals
                                       , dataset_dictionary)
{
  count_num_measurements <- function(x)
  {
    num_measurements <- sum(!is.na(x))
  }
  
  count_num_cycles <- function(x
                               , dataset_chemicals)
  {
    df_chem_cycles <- data.frame(measurements = x
                                 , SDDSRVYR = dataset_chemicals$SDDSRVYR) %>%
      na.omit(.)
    # View(df_chem_cycles)
    
    cycles_unique <- unique(df_chem_cycles$SDDSRVYR)
    # print(cycles_unique)
    
    num_cycles <- length(cycles_unique)
    
    # df_stats <- data.frame(cycles_unique = toString(cycles_unique)
    #                        , num_cycles = num_cycles)
    return(num_cycles)
    
  }
  
  count_unique_cycles <- function(x
                               , dataset_chemicals)
  {
    df_chem_cycles <- data.frame(measurements = x
                                 , SDDSRVYR = dataset_chemicals$SDDSRVYR) %>%
      na.omit(.)
    # View(df_chem_cycles)
    
    cycles_unique <- unique(df_chem_cycles$SDDSRVYR)
    # print(cycles_unique)
    
    cycles_unique_string <- toString(cycles_unique)
    
    return(cycles_unique_string)
  }
  
  df_stats_cycles <- chemicals_clean %>%
    # select(LBXBPB, LBXCOT) %>%
    map_df(.
           , count_unique_cycles
           , dataset_chemicals = dataset_chemicals) %>%
    t() %>%
    data.frame() %>%
    rename("unique_cycles" = ".") %>%
    mutate(variable_codename_use = rownames(.))
  # View(df_stats_cycles)
  
  df_stats_unique_cycles <- chemicals_clean %>%
    # select(LBXBPB, LBXCOT) %>%
    map_df(.
           , count_num_cycles
           , dataset_chemicals = dataset_chemicals) %>%
    t() %>%
    data.frame() %>%
    rename("num_cycles" = ".") %>%
    mutate(variable_codename_use = rownames(.))
  
  df_stats <- map_df(chemicals_clean
                     , count_num_measurements) %>%
    t() %>%
    data.frame() %>%
    rename("num_measurements" = ".") %>%
    mutate(variable_codename_use = rownames(.)) %>%
    relocate(variable_codename_use
             , .before = num_measurements) %>%
    full_join(.
              , df_stats_cycles
              , by = "variable_codename_use") %>%
    full_join(.
            , df_stats_unique_cycles
            , by = "variable_codename_use") %>%
    left_join(.
              , dataset_dictionary %>%
                filter(in_dataset == "chemicals")
              , by = "variable_codename_use") 
    

  View(df_stats)
}