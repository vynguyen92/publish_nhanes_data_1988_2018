consolidate_multiple_variables_into_one <- function(df_doc_cleaning
                                                    , name_correspondence_df
                                                    , list_cleaning_documentation
                                                    , df_unclean)
{
  subset_variables_derived <- df_doc_cleaning %>%
    filter(grepl("Consolidate into one column", codename_note))
  # print(subset_variables_derived)
  
  nhanes_variable_codenames <- subset_variables_derived %>%
    pull(variable_codename_use) %>%
    unique(.)
  # print(nhanes_variable_codenames)
  
  cancer_codenames <- subset_variables_derived %>%
    pull(codename_note) %>%
    gsub("Consolidate into one column, ", "", .) %>%
    unique(.)
  # print(cancer_codenames)
  
  problematic_codenames <- c(nhanes_variable_codenames
                             , cancer_codenames)
  # print(problematic_codenames)
  
  affected_cycle <- subset_variables_derived %>%
    pull(SDDSRVYR) %>%
    unique(.)
  # print(affected_cycle)
  
  df_correspondence <- list_cleaning_documentation[["Correspondence Table"]] %>%
    arrange(cancer_codes)
  
  subset_long <- df_unclean %>%
    select("SEQN"
           , "SEQN_new"
           , "SDDSRVYR"
           , all_of(problematic_codenames)) %>%
    filter(SDDSRVYR %in% affected_cycle) %>%
    pivot_longer(cols = all_of(cancer_codenames)
                 , names_to = "cancer_codename"
                 , values_to = "cancer_codes") %>%
    pivot_longer(cols = all_of(nhanes_variable_codenames)
                 , names_to = "variable_codename"
                 , values_to = "variable_values") %>%
    drop_na(variable_values) %>%
    drop_na(cancer_codes) %>%
    mutate(letter_cancer_codenames = gsub("MCQ230","", cancer_codename)) %>%
    mutate(letter_variable_codename_use = gsub("MCD240","", variable_codename)) %>%
    mutate(match_question_value = letter_cancer_codenames == letter_variable_codename_use) %>%
    filter(match_question_value == TRUE) %>%
    select(-letter_cancer_codenames
           , -letter_variable_codename_use
           , -match_question_value) %>%
    left_join(.
              , df_correspondence
              , by = "cancer_codes")
  # View(subset_long)
  
  df_checking_categories <- subset_long %>%
    select(variable_codename_use
           , variable_description_use
           , cancer_codes
           , cancer_code_description) %>%
    unique(.) %>%
    arrange(cancer_codes) %>%
    filter(cancer_codes != 99)
  # print("df_checking_categories")
  # print(str(df_checking_categories))
  
  cancer_codes_not_in_recorded <- outersect(df_correspondence$cancer_code_description
                                            , df_checking_categories$cancer_code_description)
  
  
  num_not_recorded_cancer_codes <- length(cancer_codes_not_in_recorded)
  
  if(num_not_recorded_cancer_codes > 0)
  {
    # print(cancer_codes_not_in_recorded)
    
    df_correspondence <- df_correspondence %>%
      filter(!(cancer_code_description %in% cancer_codes_not_in_recorded))
    # View(df_correspondence)
    
    # View(df_checking_categories)
    
    same_cancer_codes <- identical(df_correspondence
                                   , df_checking_categories)
    
    if(same_cancer_codes == FALSE)
    {
      different_variables <- outersect(df_correspondence$cancer_code_description
                                       , df_checking_categories$cancer_code_description)
      print(different_variables)
    }
    
    # print(df_correspondence$variable_codename_use==df_checking_categories$variable_codename_use)
  }
  
  # print(colnames(subset_long))
  
  subset_long_updated <- subset_long
  
  subset_long <- subset_long %>%
    select(SEQN
           , SEQN_new
           , SDDSRVYR
           , variable_codename_use
           , variable_values) %>%
    mutate(variable_values = as.numeric(variable_values)) %>%
    drop_na(variable_codename_use) 
  # View(subset_long)
  
  subset_wide <- subset_long %>%
    pivot_wider(names_from = variable_codename_use
                , values_from = variable_values)
  # View(subset_wide)
  
  
  
  index_colnames_affected <- which(!(colnames(subset_wide) %in% c("SEQN"
                                                                  , "SEQN_new"
                                                                  , "SDDSRVYR")))
  
  colnames_affected <- colnames(subset_wide)[index_colnames_affected]
  # print(colnames_affected)
  
  subset_nhanes_cycle_10 <- df_unclean %>%
    select(-all_of(colnames_affected)) %>%
    filter(SDDSRVYR == 10) %>%
    full_join(.
              , subset_wide %>%
                select(!c("SEQN_new"
                          , "SDDSRVYR"))
              , by = "SEQN") %>%
    as.data.table(.)
  # print(colnames(subset_nhanes_cycle_10))
  # View(subset_nhanes_cycle_10)
  
  # Checking merging of code values
  subset_long_checking <- subset_long_updated %>%
    select(SEQN_new
           , cancer_codes
           , variable_values) %>%
    filter(cancer_codes != 99) %>%
    mutate(variable_values = as.numeric(variable_values)) %>%
    # filter(variable_values != 99999) %>%
    arrange(SEQN_new
            , cancer_codes)
  # print("subset_long_checking")
  # print(str(subset_long_checking))


  subset_nhanes_cycle_10_checking <- subset_nhanes_cycle_10 %>%
    select(SEQN_new
           , all_of(colnames_affected)) %>%
    pivot_longer(cols = all_of(colnames_affected)
                 , names_to = "variable_codename_use"
                 , values_to = "variable_values") %>%
    drop_na(variable_values) %>%
    left_join(.
              , df_correspondence
              , by = "variable_codename_use") %>%
    select(SEQN_new
           , cancer_codes
           , variable_values) %>%
    arrange(SEQN_new
            , cancer_codes)

  # print(str(subset_long_checking))
  # print(str(subset_nhanes_cycle_10_checking))

  same_df_cancer_codes_values <- identical(subset_long_checking
                                           , subset_nhanes_cycle_10_checking)

  # if(same_df_cancer_codes_values == FALSE)
  # {
  #   nrow_subset_long_checking <- nrow(subset_long_checking)
  #   print(nrow_subset_long_checking)
  #   nrow_subset_nhanes_cycle_10_checking <- nrow(subset_nhanes_cycle_10_checking)
  #   print(nrow_subset_nhanes_cycle_10_checking)
  # 
  #   index_codes <- which(subset_long_checking$cancer_codes != subset_nhanes_cycle_10_checking$cancer_codes)
  #   print(index_codes)
  # 
  #   index_values <- which(subset_long_checking$variable_values != subset_nhanes_cycle_10_checking$variable_values)
  #   print(index_values)
  # 
  #   View(subset_long_checking)
  #   View(subset_nhanes_cycle_10_checking)
  # }

  rm(subset_long)
  rm(subset_long_checking)
  rm(subset_nhanes_cycle_10_checking)

  library(data.table)
  
  df_clean <- rbindlist(list("subset_nhanes_rest" = df_unclean %>%
                               filter(SDDSRVYR != 10) %>%
                               as.data.table(.)
                             , "subset_nhanes_cycle_10" = subset_nhanes_cycle_10 %>%
                               as.data.table(.))
                        , use.names = TRUE)

  # nrow_subset_nhanes_rest <- nrow(list_clean_df[["subset_nhanes_rest"]])
  # nrow_subset_nhanes_cycle_10 <- nrow(list_clean_df[["subset_nhanes_cycle_10"]])
  # nrow_df_clean <- nrow(df_clean)
  # 
  # num_rows_same <- nrow_subset_nhanes_rest + nrow_subset_nhanes_cycle_10 == nrow_df_clean
  # 
  # ncol_subset_nhanes_rest <- ncol(list_clean_df[["subset_nhanes_rest"]])
  # ncol_subset_nhanes_cycle_10 <- ncol(list_clean_df[["subset_nhanes_cycle_10"]])
  # ncol_df_clean <- ncol(df_clean)
  # 
  # num_cols_same <- ncol_subset_nhanes_rest == ncol_subset_nhanes_cycle_10 &
  #   ncol_subset_nhanes_cycle_10 == ncol_df_clean &
  #   ncol_subset_nhanes_rest == ncol_df_clean
  # 
  # if(num_rows_same == FALSE | num_cols_same == FALSE)
  # {
  #   print(duplicated(df_clean$SEQN))
  # 
  #   colnames_diff_rest_merge<- outersect(colnames(list_clean_df[["subset_nhanes_rest"]])
  #                                        , colnames(df_clean))
  #   print(colnames_diff_rest_merge)
  #   colnames_diff_10_merge <- outersect(colnames(list_clean_df[["subset_nhanes_cycle_10"]])
  #                                       , colnames(df_clean))
  #   print(colnames_diff_10_merge)
  # 
  #   print(colnames(df_clean)[grepl(".x$|.y$", colnames(df_clean))])
  # }

  gc()
  
  return(as.data.frame(df_clean))
}