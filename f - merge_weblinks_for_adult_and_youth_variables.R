merge_weblinks_for_adult_and_youth_variables <- function(df_documentation_weblinks)
{
  df_documentation_weblinks_withOUT_adult_youth <- df_documentation_weblinks %>%
    filter(!(file_name %in% c("ADULT", "YOUTH")))
  # print(unique(df_documentation_weblinks_withOUT_adult_youth$file_name))
  
  df_documentation_weblinks_with_adult_youth <- df_documentation_weblinks %>%
    filter(file_name %in% c("ADULT", "YOUTH")) %>%
    mutate(original_codenames = paste(original_codenames
                                      , " ("
                                      , file_name
                                      , ")"
                                      , sep = "")) 
  # View(df_documentation_weblinks_with_adult_youth)
  
  df_documentation_weblinks_with_adult_youth_collapsed <- df_documentation_weblinks_with_adult_youth %>%
    group_by(variable_codename_use) %>%
    select(-file_name) %>%
    mutate(original_codenames = paste(original_codenames
                                      , collapse = ", ")) %>%
    mutate(weblink = paste(weblink
                           , collapse = ", ")) %>%
    mutate(original_codenames = ifelse(grepl(", ", original_codenames) == FALSE
                                       , gsub(" \\(ADULT\\)| \\(YOUTH\\)", "", original_codenames)
                                       , original_codenames))
  # View(df_documentation_weblinks_with_adult_youth_collapsed)
  
  df_documentation_weblinks_final <- df_documentation_weblinks_withOUT_adult_youth %>%
    full_join(.
              , df_documentation_weblinks_with_adult_youth_collapsed
              , by = NULL)
  # View(df_documentation_weblinks_final)
  
  return(df_documentation_weblinks_final)
}