check_new_codename <- function(list_documentation
                               , name_of_dataset)
{
  df_documentation <- list_documentation[[name_of_dataset]]
  # View(df_documentation)
  
  name_dataset_fix_cats <- paste(name_of_dataset
                                 , "Fix Category")
  
  df_documentation_fix_categories <- list_documentation[[name_dataset_fix_cats]]
  # View(df_documentation_fix_categories)
  
  codenames_fix <- df_documentation_fix_categories %>%
    pull(new_codename) %>%
    unique(.)
  
  codenames_harmonized <- df_documentation %>%
    pull(variable_codename_use) %>%
    unique(.)
  
  setdiff(codenames_fix
          , codenames_harmonized) %>%
    data.frame(.) %>%
    View(.)
  
}