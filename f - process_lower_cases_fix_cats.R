process_lower_cases_fix_cats <- function(df_fix_categories)
{
  # View(df_fix_categories)
  
  subset_fix_categories <- df_fix_categories %>%
    filter(grepl("^MCQ", new_codename) == TRUE) %>%
    filter(grepl("[a-z]$", codename_original) == TRUE)
  # View(subset_fix_categories)
  
  problematic_codenames <- subset_fix_categories %>%
    pull(new_codename) %>%
    unique(.) 
  # print(problematic_codenames)
  
  num_problematic_codenames <- length(problematic_codenames)
  # print(num_problematic_codenames)
  
  list_problem <- list()
  
  for(i in seq(num_problematic_codenames))
  {
    prob_codename_i <- problematic_codenames[i]
    # print(prob_codename_i)
    
    subset_i <- df_fix_categories %>%
      filter(new_codename == prob_codename_i)
    # print(subset_i)
    
    codenames_lower_upper_case <- subset_i %>%
      filter(grepl("[a-z]$", codename_original) == TRUE) %>%
      pull(codename_original) %>%
      unique(.) %>%
      append(.
             , toupper(.))
    # print(codenames_lower_upper_case)
    
    subset_other <- subset_i %>%
      filter(!(codename_original %in% codenames_lower_upper_case))
    # print(subset_other)
    
    subset_problem <- subset_i %>%
      filter(codename_original %in% codenames_lower_upper_case) %>%
      mutate(codename_original = toupper(codename_original)) %>%
      pivot_longer(cols = grep("^cycle"
                               , colnames(.))
                   , names_to = "cycle"
                   , values_to = "value") %>%
      unique(.) %>%
      drop_na(value) %>%
      pivot_wider(names_from = "cycle"
                  , values_from = "value")
    # View(long_problem)
    
    subset_together <- subset_other %>%
      full_join(.
                , subset_problem
                , by = NULL)
    # View(subset_together)
    
    list_problem[[i]] <- subset_together
  }
  
  df_not_problem <- df_fix_categories %>%
    filter(!(new_codename %in% problematic_codenames))
  # View(df_not_problem)
  
  # Define a function to merge the datasets by column names
  joining_by_colnames <- function(x, y) full_join(x
                                                  , y
                                                  , by = NULL)
  
  df_problem <- list_problem %>%
    reduce(joining_by_colnames)
  # View(df_problem)
  
  df_together <- df_not_problem %>%
    full_join(.
              , df_problem
              , by = NULL)
  
  write_csv(df_together, "questionnaire_fix_categories_no_lower_case.csv")
  
}