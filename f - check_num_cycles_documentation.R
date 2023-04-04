check_num_cycles_documentation <- function(list_documentation
                                           , name_of_dataset)
{
  df_documentation <- list_documentation[[name_of_dataset]]
  # View(df_documentation)
  
  name_dataset_fix_cats <- paste(name_of_dataset
                                 , "Fix Category")
  
  df_documentation_fix_categories <- list_documentation[[name_dataset_fix_cats]]
  # View(df_documentation_fix_categories)
  
  df_doc_fix_cycles <- df_documentation_fix_categories %>%
    select(codename_original
           , grep("^cycle"
                  , colnames(.))) %>%
    pivot_longer(cols = grep("^cycle"
                             , colnames(.))
                 , names_to = "SDDSRVYR"
                 , values_to = "value_fix_categories") %>%
    mutate(SDDSRVYR = gsub("cycle_"
                        , ""
                        , SDDSRVYR)) %>%
    drop_na(value_fix_categories) %>%
    mutate(SDDSRVYR = as.numeric(SDDSRVYR)) %>%
    rename(variable_codename = codename_original)
  # View(df_doc_fix_cycles)
  
  df_doc_cycles <- df_documentation %>%
    select(variable_codename
           , SDDSRVYR
           , variable_codename_use) %>%
    unique(.)
  # View(df_doc_cycles)
  
  df_doc_togther <- df_doc_cycles %>%
    full_join(.
              , df_doc_fix_cycles
              , by = c("variable_codename"
                       , "SDDSRVYR"))
  View(df_doc_togther)
}