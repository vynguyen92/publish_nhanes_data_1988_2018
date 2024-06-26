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
                                           , name_of_dataset
                                           , list_cleaning_documentation
                                           , dataset_unclean)
{
  library(purrr)
  library(dplyr)
  
  dataset_doc <- list_cleaning_documentation[[name_of_dataset]]
  # View(dataset_doc)
  
  # print(colnames(dataset_unclean))
  # Extract the dataset on cleaning documentation for harmonizing categories over time
  dataset_fix_categories <- list_cleaning_documentation[[name_fix_categories_of_df]]
  # View(dataset_fix_categories)
  
  # Determine the codenames that need harmonizing
  new_codenames_fix <- dataset_fix_categories %>%
    pull(new_codename) %>%
    unique(.) #%>%
    # .[1]
  # print(new_codenames_fix)
  
  # new_codenames_fix <- c( "RHD180"
  #                        , "RHD190"
  #                        , "RHQ171"
  #                        , "RHQ542B"
  #                        , "RHQ542C"
  #                        )
  
  # new_codenames_fix <- c("DID040", "DIQ060")

  # new_codenames_fix <- "MCD180D"

  list_harmonized_subsets <- list()
  
  # Determine the number of codenames that need harmonizing
  num_new_codenames_fix <- length(new_codenames_fix)
  # print(num_new_codenames_fix)

  # View(dataset_unclean %>%
  #        select("SDDSRVYR"
  #               , "DIQ060"
  #               , "DIQ060G"
  #               , "DIQ060Q") %>%
  #        unique(.))
  
  for(f in seq(num_new_codenames_fix))
  {
    # Determine a given codename that requires harmonizing of its categories
    new_codename_fix_f <- new_codenames_fix[f]
    # print(new_codename_fix_f)

    subset_documentation <- dataset_doc %>%
      filter(variable_codename_use == new_codename_fix_f)
    # print(subset_documentation)
    
    # Extract information on the number and description of the categories of this categorical variable
    subset_category_fix <- dataset_fix_categories %>%
      filter(new_codename == new_codename_fix_f)
    # print(subset_category_fix)
    
    is_derived_variable <- subset_documentation %>%
      filter(grepl("Derive", codename_note) == TRUE) %>%
      pull(codename_note) %>%
      unique(.) %>%
      length(.)
    # print(is_derived_variable)
    
    if(is_derived_variable == 1)
    {
      questionnaire_codename <- subset_category_fix %>%
        pull(codename_original) %>%
        unique(.) %>%
        .[grepl("G$",.)]
      # print(questionnaire_codename)
      
      subset_category_fix <- subset_category_fix %>%
        filter(codename_original != questionnaire_codename)
      
    }
    # print(subset_category_fix)

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
    # print(str(long_subset_category_fix))
    
    num_categories_with_ranges <- nrow(subset_category_fix_ranges)
    
    if(num_categories_with_ranges != 0)
    {
      long_subset_category_fix <- incorporate_categorical_changes_for_ranges(long_subset_category_fix
                                                                             , subset_category_fix_ranges)
    }
    # print(str(long_subset_category_fix))
    # View(long_subset_category_fix)

    # Determine the column index with the name "categories_num"
    col_index_categories_num <- which(colnames(long_subset_category_fix) == "categories_num")

    # Change that column name to the new codename
    colnames(long_subset_category_fix)[col_index_categories_num] <- new_codename_fix_f
    # print(new_codename_fix_f)
    # print(colnames(long_subset_category_fix)[col_index_categories_num])
    # View(long_subset_category_fix)
    # print(str(long_subset_category_fix))

    # Remove the column of the new codenames that will contain the harmonized categories as it is
    # used as the column name of the old categories
    long_subset_category_fix <- long_subset_category_fix %>%
      select(-new_codename)
    # print(str(long_subset_category_fix))

    # Extract the original codenames. Use for debugging and checking.
    original_codenames <- subset_category_fix %>%
      select(codename_original) %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    # print(original_codenames)
    
    if(new_codename_fix_f == "DMDEDUC2")
    {
      long_subset_category_fix <- long_subset_category_fix %>%
        mutate(age_group = "ADULTS")

      subset_unclean <- dataset_unclean %>%
        select("SEQN_new"
               , "age_group"
               , new_codename_fix_f
               , all_of(original_codenames)
               , "SDDSRVYR") %>%
        left_join(.
                  , long_subset_category_fix
                  , by = c(new_codename_fix_f
                           , "age_group"
                           , "SDDSRVYR"))

    } else if(new_codename_fix_f == "DMDEDUC3") {

      long_subset_category_fix <- long_subset_category_fix %>%
        mutate(age_group = "YOUTH")

      subset_unclean <- dataset_unclean %>%
        select("SEQN_new"
               , "age_group"
               , new_codename_fix_f
               , all_of(original_codenames)
               , "SDDSRVYR") %>%
        left_join(.
                  , long_subset_category_fix
                  , by = c(new_codename_fix_f
                           , "age_group"
                           , "SDDSRVYR"))

    } else {

      subset_unclean <- dataset_unclean %>%
        select( "SEQN_new"
               , new_codename_fix_f
               , all_of(original_codenames)
               , "SDDSRVYR") %>%
        left_join(.
                  , long_subset_category_fix
                  , by = c(new_codename_fix_f
                           , "SDDSRVYR"))
    }
    
    # # Used for checking
    # View(subset_unclean  %>%
    #        select(-c(SEQN_new
    #                  # , SDDSRVYR
    #                  )) %>%
    #        unique(.))

    # print(new_codename_fix_f)
    subset_unclean <- subset_unclean %>%
      select(-all_of(new_codename_fix_f))
    # print(colnames(subset_unclean))
    
   
    
    # View(subset_unclean)

    # index_column_old_categories <- which(colnames(subset_unclean) == new_codename_fix_f)
    # print(index_column_old_categories)

    index_column_new_categories <- which(colnames(subset_unclean) == "new_categories")
    colnames(subset_unclean)[index_column_new_categories] <- new_codename_fix_f
    # print(new_codename_fix_f)
    # print(colnames(subset_unclean))
    
    
    
    if(new_codename_fix_f == "DMDEDUC3")
    {
      df_checking <- subset_unclean %>%
        filter(age_group == "YOUTH") %>%
        select(all_of(new_codename_fix_f)
               , all_of(original_codenames)
               , "age_group"
               , "SDDSRVYR") %>%
        unique(.)

    } else if(new_codename_fix_f == "DMDEDUC2") {

      df_checking <- subset_unclean %>%
        filter(age_group == "ADULTS") %>%
        select(all_of(new_codename_fix_f)
               , all_of(original_codenames)
               , "age_group"
               , "SDDSRVYR") %>%
        unique(.)

    } else {
      df_checking <- subset_unclean %>%
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

    
    # Problematic variables for the questionnaire module include RHQ542B and RHQ542C, but these are okay
    # because these variables are only concern about use of products (e.g., patches or Cream/suppository/injection)
    # instead of not using (MAPF45 or MAPF41 == 2) or don't know (MAPF45 or MAPF41 == 9)
    if(all_na_in_df_checking_problems == FALSE)
    {
      print(new_codename_fix_f)
      print(df_checking_problems)
    }
    
    list_harmonized_subsets[[f]] <- subset_unclean %>%
      select(-SDDSRVYR)
    
  }

  # View(str(list_harmonized_subsets))
  
  dataset_unclean_harmonized <- reduce(list_harmonized_subsets
                                       , full_join
                                       , by = c("SEQN_new"))
  # print(dim(dataset_unclean_harmonized))

  # There were duplicates for HFA8R and age_group, since two different demographic
  # variables used it for adults and youth.
  if(name_fix_categories_of_df == "Demographics Fix Category")
  {
    extra_colnames <- colnames(dataset_unclean_harmonized) %>%
      grep("\\.(x|y)$", ., value = TRUE)
    # print(extra_colnames)

    dataset_unclean_harmonized <- dataset_unclean_harmonized %>%
      select(-one_of(extra_colnames))
  }

  # print(dim(dataset_unclean_harmonized))

  colnames_other_unaffected <- setdiff(colnames(dataset_unclean)
                                       , colnames(dataset_unclean_harmonized)) %>%
    append(.
           , c("SEQN_new"))
  # print(colnames_other_unaffected)

  dataset_unclean_unaffected <- dataset_unclean %>%
    select(all_of(colnames_other_unaffected))

  list_clean <- list(dataset_unclean_unaffected
                    , dataset_unclean_harmonized)
  # print(str(list_clean))
  
  # Define the dataset as clean
  dataset_clean <- reduce(list_clean
                          , full_join
                          , by = "SEQN_new")

  # print(dim(dataset_clean))

  # print(colnames(dataset_unclean_harmonized))
  # print(colnames(dataset_unclean_unaffected))

  diff_num_participants <- nrow(dataset_clean) - nrow(dataset_unclean)

  diff_num_colnames <- ncol(dataset_clean) - ncol(dataset_unclean)

  # Unit test to detect if extra variables or participants were added during the
  # harmonization of the categories
  if(diff_num_participants != 0 | diff_num_colnames != 0)
  {
    seqn_unclean <- dataset_unclean$SEQN_new
    seqn_clean <- dataset_clean$SEQN_new
    print(outersect(seqn_unclean, seqn_clean))

    colnames_unclean <- colnames(dataset_unclean)
    colnames_clean <- colnames(dataset_clean)
    print(outersect(colnames_unclean, colnames_clean))
  }
  
  return(dataset_clean)
}