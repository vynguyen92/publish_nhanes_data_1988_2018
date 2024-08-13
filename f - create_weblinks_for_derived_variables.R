create_weblinks_for_derived_variables <- function(df_documentation_weblinks
                                                  , df_file_name_subsection_nhanes_iii)
{
  library("tidyverse")
  
  subset_derived_variables <- df_documentation_weblinks %>% 
    filter(is.na(file_name))
  # View(subset_derived_variables)
  
  df_documentation_weblinks_updated <- df_documentation_weblinks
  
  num_variables_cycles <- nrow(subset_derived_variables)
  
  for(i in seq(num_variables_cycles))
  {
    subset_derived_variable_i <- subset_derived_variables[i,]
    # print(subset_derived_variable_i)
    
    derived_variable_i <- subset_derived_variable_i %>%
      pull(variable_codename_use)
    # print(derived_variable_i)
    # print(i)
    
    study_years_i <- subset_derived_variable_i %>%
      pull(year) 
    # print(study_years_i)
    
    replicated_codenames <- subset_derived_variable_i %>%
      pull(replicate_codenames)
    # print(replicated_codenames)
    
    num_replicated_codenames <- replicated_codenames %>%
      strsplit(.
               , split = ", ") %>%
      unlist(.) %>%
      length(.)
    # print(num_replicated_codenames)
    
    subset_replicated_codenames <- subset_derived_variable_i %>%
      select(variable_codename_use
             , SDDSRVYR
             , year
             , replicate_codenames) %>%
      separate(col = replicate_codenames
               , into = paste("variable_used_for_deriving"
                              , seq(num_replicated_codenames)
                              , sep = "_")
               , sep = ", ") %>%
      pivot_longer(.
                   , cols = starts_with("variable_used_for_deriving")
                   , names_to = "column_names"
                   , values_to = "variable_used_for_deriving") %>%
      select(-column_names) %>%
      left_join(.
                , df_documentation_weblinks %>% 
                  filter(!is.na(file_name)) %>%
                  rename(variable_used_for_deriving = variable_codename_use) %>%
                  select(variable_used_for_deriving
                         , variable_codename
                         , file_name
                         , year)
                , by = c("variable_used_for_deriving"
                         , "year")) %>%
      left_join(.
                , df_file_name_subsection_nhanes_iii
                , by = "file_name") %>%
      mutate(weblink = ifelse(SDDSRVYR == -1
                              , paste("https://wwwn.cdc.gov/nchs/data/nhanes3/"
                                      , subsection
                                      , "/"
                                      , file_name %>%
                                        tolower(.)
                                      , "-acc"
                                      , ifelse(file_name == "CMV"
                                               , ".htm"
                                               , ".pdf")
                                      , sep = "")
                              ,  paste("https://wwwn.cdc.gov/Nchs/Nhanes/"
                                       , year
                                       , "/"
                                       , file_name
                                       , ".htm"
                                       , sep = "")))
    
    if(num_replicated_codenames != nrow(subset_replicated_codenames))
    {
      df_not_problematic_derived_variables <- data.frame("derived_variable" = c("LBXPFOA"
                                                                              , "LBXPFOS")
                                                         , "study_years" = c("2013-2014"
                                                                           , "2013-2014"))
      
      subset_not_problematic <- df_not_problematic_derived_variables %>%
        filter(derived_variable == derived_variable_i) %>%
        filter(study_years == study_years_i)
      # print(subset_not_problematic)
      
      # If this object returns a 1, then the derived variable is NOT problematic (TRUE)
      derived_variable_is_not_problematic <- nrow(subset_not_problematic)
      # print(derived_variable_is_not_problematic)
      
      if(derived_variable_is_not_problematic != 1)
      {
        print("ERROR")
        print(derived_variable_i)
        print(study_years_i)
        print(replicated_codenames)
        print(subset_replicated_codenames)
      }
      

    }
    
    collapsed_original_variables <- subset_replicated_codenames %>%
      pull(variable_codename) %>%
      paste(.
            , collapse = ", ")
    # print(collapsed_original_variables)
    
    collapsed_weblinks <- subset_replicated_codenames %>%
      pull(weblink) %>%
      paste(.
            , collapse = ", ")
    # print(collapsed_weblinks)
    
    index_variable_cycle_i <- which(df_documentation_weblinks_updated$variable_codename_use == derived_variable_i
                                    & df_documentation_weblinks_updated$year == study_years_i)
    # print(index_variable_cycle_i)
    
    df_documentation_weblinks_updated[index_variable_cycle_i, "original_codenames"] <- collapsed_original_variables
    
    df_documentation_weblinks_updated[index_variable_cycle_i, "weblink"] <- collapsed_weblinks
    
    # print(df_documentation_weblinks_updated[index_variable_cycle_i, ])
    
  }
  View(df_documentation_weblinks_updated)
}