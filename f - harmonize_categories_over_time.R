#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#################################  FUNCTION TO HARMONIZE THE CATEGORIES OVER TIME  ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function is to include a column with the harmonized codenames for ease of extraction.
#
# Inputs: name_fix_categories_of_df - string of the cleaning documentation dataset on fixing the categories
#         list_cleaning_documentation - list of the datasets on cleaning documentation
#         dataset_unclean - dataframe of the NHANES dataset with the codename harmonized (i.e. there is one
#                           codename per one variable)
#
# Outputs: dataset_clean - dataframe of the cleaning documentation with a new column of harmonized codename

harmonize_categories_over_time <- function(name_fix_categories_of_df
                                           , list_cleaning_documentation
                                           , dataset_unclean)
{
  
  # Extract the dataset on cleaning documentation for harmonizing categories over time
  dataset_fix_categories <- list_cleaning_documentation[[name_fix_categories_of_df]]
  # View(dataset_fix_categories)
  
  # Determine the codenames that need harmonizing
  new_codenames_fix <- dataset_fix_categories %>%
    pull(new_codename) %>%
    unique(.) #%>%
    # .[1]
  # print(new_codenames_fix)
  
  # new_codenames_fix <- c("DMDHRAGE"
  #                        , "DMDYRSUS"
  #                        , "DMDHRBR4"
  #                        )
  
  # new_codenames_fix <- "DMDYRSUS"

  # new_codenames_fix <- "DMDEDUC3"

  # Determine the number of codenames that need harmonizing
  num_new_codenames_fix <- length(new_codenames_fix)
  # print(num_new_codenames_fix)

  for(f in seq(num_new_codenames_fix))
  {
    # Determine a given codename that requires harmonizing of its categories
    new_codename_fix_f <- new_codenames_fix[f]
    # print(new_codename_fix_f)

    # Extract information on the number and description of the categories of this categorical variable
    subset_category_fix <- dataset_fix_categories %>%
      filter(new_codename == new_codename_fix_f)

    subset_category_fix_ranges <- subset_category_fix %>%
      filter(range_of_values == 1)
    # View(subset_category_fix_ranges)

    subset_category_fix <- subset_category_fix %>%
      filter(is.na(range_of_values) == TRUE)
    # View(subset_category_fix)

    # Format the extraction data as long with a column for the old category numbers, the new category numbers,
    # the new codename, and the cycle
    long_subset_category_fix <- subset_category_fix %>%
      gather(., year, value, grep("cycle", colnames(.))) %>%
      drop_na(value) %>%
      mutate(SDDSRVYR = as.numeric(gsub("cycle_"
                                        , ""
                                        , year))) %>%
      select(categories_num, new_categories, new_codename, SDDSRVYR) %>%
      mutate(categories_num = as.numeric(categories_num)) %>%
      mutate(new_categories = as.numeric(new_categories)) %>%
      unique(.)
    # View(long_subset_category_fix)
    # # print(str(long_subset_category_fix))
    
    num_categories_with_ranges <- nrow(subset_category_fix_ranges)
    
    if(num_categories_with_ranges != 0)
    {
      long_subset_category_fix <- incorporate_categorical_changes_for_ranges(long_subset_category_fix
                                                                             , subset_category_fix_ranges)
    }
    # View(long_subset_category_fix)

    # Determine the column index with the name "categories_num"
    col_index_categories_num <- which(colnames(long_subset_category_fix) == "categories_num")

    # Change that column name to the new codename
    colnames(long_subset_category_fix)[col_index_categories_num] <- new_codename_fix_f
    # print(colnames(long_subset_category_fix)[col_index_categories_num])
    # View(long_subset_category_fix)

    # Remove the column of the new codenames that will contain the harmonized categories as it is
    # used as the column name of the old categories
    long_subset_category_fix <- long_subset_category_fix %>%
      select(-new_codename)
    # View(long_subset_category_fix)

    if(new_codename_fix_f == "DMDEDUC2")
    {
      long_subset_category_fix <- long_subset_category_fix %>%
        mutate(age_group = "ADULTS")

      dataset_unclean <- dataset_unclean %>%
        left_join(.
                  , long_subset_category_fix
                  , by = c(new_codename_fix_f
                           , "age_group"
                           , "SDDSRVYR"))

    } else if(new_codename_fix_f == "DMDEDUC3") {

      long_subset_category_fix <- long_subset_category_fix %>%
        mutate(age_group = "YOUTH")

      dataset_unclean <- dataset_unclean %>%
        left_join(.
                  , long_subset_category_fix
                  , by = c(new_codename_fix_f
                           , "age_group"
                           , "SDDSRVYR"))

    } else {
      
      dataset_unclean <- dataset_unclean %>%
        left_join(.
                  , long_subset_category_fix
                  , by = c(new_codename_fix_f
                           , "SDDSRVYR"))
    }

    # Extract the original codenames. Use for debugging and checking.
    original_codenames <- subset_category_fix %>%
      select(codename_original) %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    # print(original_codenames)

    # View(dataset_unclean %>%
    #        select("new_categories"
    #               , all_of(new_codename_fix_f)
    #               , all_of(original_codenames)
    #               # , "age_group"
    #               , "SDDSRVYR"
    #               ) %>%
    #        unique(.))

    dataset_unclean <- dataset_unclean %>%
      select(-all_of(new_codename_fix_f))

    # index_column_old_categories <- which(colnames(dataset_unclean) == new_codename_fix_f)
    # print(index_column_old_categories)

    index_column_new_categories <- which(colnames(dataset_unclean) == "new_categories")
    colnames(dataset_unclean)[index_column_new_categories] <- new_codename_fix_f

    if(new_codename_fix_f == "DMDEDUC3")
    {
      df_checking <- dataset_unclean %>%
        filter(age_group == "YOUTH") %>%
        select(all_of(new_codename_fix_f)
               , all_of(original_codenames)
               , "age_group"
               , "SDDSRVYR") %>%
        unique(.)
      
    } else if(new_codename_fix_f == "DMDEDUC2") {
    
      df_checking <- dataset_unclean %>%
        filter(age_group == "ADULTS") %>%
        select(all_of(new_codename_fix_f)
               , all_of(original_codenames)
               , "age_group"
               , "SDDSRVYR") %>%
        unique(.)
      
    } else {
      df_checking <- dataset_unclean %>%
        select(all_of(new_codename_fix_f)
               , all_of(original_codenames)
               , "SDDSRVYR") %>%
        unique(.)
    }
    # View(df_checking)
    
    index_problems <- which(is.na(df_checking[,new_codename_fix_f]) == TRUE)
    
    df_checking_problems <- df_checking[index_problems,]
    
    df_checking_problems_no_cycle <- df_checking_problems %>%
      select(-SDDSRVYR)
    # print(df_checking_problems_no_cycle)
    
    if(new_codename_fix_f %in% c("DMDEDUC2"
                                 , "DMDEDUC3"))
    {
      df_checking_problems_no_cycle <- df_checking_problems_no_cycle %>%
        select(-age_group)
    }
    
    all_na_in_df_checking_problems <- all(is.na(df_checking_problems_no_cycle))
    
    if(all_na_in_df_checking_problems == FALSE)
    {
      print(new_codename_fix_f)
      print(df_checking_problems)
    }
    
  }

  # Define the dataset as clean
  dataset_clean <- dataset_unclean
  
  return(dataset_clean)
}