eliminate_fix_categories_same <- function(df_fix_cats)
{
  df_stats <- df_fix_cats %>%
    group_by(new_codename) %>%
    summarise(same = length(unique(categories_num == new_categories))) %>%
    ungroup()
  # View(df_stats)
  
  codenames_diff_cats <- df_stats %>%
    filter(same == 2) %>%
    pull(new_codename)
  # print(codenames_diff_cats)
    
  df_fix_cats_updated <- df_fix_cats %>%
    filter(new_codename %in% codenames_diff_cats)
  # print(unique(df_fix_cats_updated$new_codename))
  # View(df_fix_cats_updated)
  
  write.csv(df_stats
            , "questionnaire_fix_categories_diff.csv")
}