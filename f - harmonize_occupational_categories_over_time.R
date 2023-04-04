#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################  FUNCTION TO HARMONIZE THE CATEGORIES OVER TIME FOR THE OCCUPATIONAL VARIABLES  ##############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function is to include a column with the harmonized categories for the occupational variables
#          If the occupational variable describe a participant job, then this function form a column to 
#          categorizes the participant as blue or white collar.
#
# Inputs: name_fix_categories_of_df - string of the cleaning documentation dataset on fixing the categories
#         list_cleaning_documentation - list of the datasets on cleaning documentation
#         dataset_unclean - dataframe of the NHANES dataset with the codename harmonized (i.e. there is one
#                           codename per one variable)
#
# Outputs: dataset_clean - dataframe of the cleaning documentation with a new column of harmonized codename

harmonize_occupational_categories_over_time <- function(name_fix_categories_of_df
                                                        , list_cleaning_documentation
                                                        , dataset_unclean)
{
  # Extract the dataset on cleaning documentation for harmonizing categories over time 
  dataset_fix_categories <- list_cleaning_documentation[[name_fix_categories_of_df]]
  # print(dataset_fix_categories)
  
  # Determine the set of two codenames that will contain the harmonized categories for the occupational variables
  new_codenames_fix <- unique(dataset_fix_categories$new_codename)#[3]
  # print(new_codenames_fix)
  
  # Determine the number of codenames that need harmonizing
  num_new_codenames_fix <- length(new_codenames_fix)
  
  for(f in seq(num_new_codenames_fix))
  {
    # Determine a given set of two codenames that will contain the harmonized categories for the occupational variables
    new_codename_fix_f <- new_codenames_fix[f]
    # print(new_codename_fix_f)
    
    # Extract information on the number and description of the categories of this set of occupational codenames
    subset_category_fix <- dataset_fix_categories %>%
      filter(new_codename == new_codename_fix_f)
    # print(subset_category_fix)
    
    # Format the extraction data as long with a column for the old category numbers and description, 
    # the new category numbers and description, the new codename for the occupational variable, 
    # the collar categories, the new codename for the collar categories and the cycle 
    long_subset_category_fix <- subset_category_fix %>%
      gather(., year, value, grep("cycle", colnames(.))) %>%
      filter(is.na(value) == FALSE) %>%
      mutate(SDDSRVYR = as.numeric(gsub("cycle_"
                                        , ""
                                        , year))) %>%
      select(codename_original
             , categories_num
             , categories_description
             , new_categories
             , new_categories_description
             , new_codename
             , collar_categories
             , new_codename_collar
             , SDDSRVYR) %>%
      unique(.) %>%
      # Remove the columns pertaining to the collar categories when working with the industry variables
      .[, colSums(is.na(.)) != nrow(.)]
    # View(long_subset_category_fix)
    
    # Determine original codenames that with categories that need to be harmonized
    old_codenames <- unique(long_subset_category_fix$codename_original)
    
    # Determine the number of original codenames
    num_old_codenames <- length(old_codenames)
    # print(old_codenames)
    
    # Intialize an empty vector to contain the harmonized occupational categories 
    vector_new_codename <- rep(NA, nrow(dataset_unclean))
    
    for(o in seq(num_old_codenames))
    {
      # Determine an original codename
      old_codename_o <- old_codenames[o]
      # print(old_codename_o)
      
      # Determine the row indices for participants who have a value for an original occupational codename
      index_with_old_codename <- which(is.na(dataset_unclean[,old_codename_o]) == FALSE)
      
      # Take the value for an original occupational codename and put it in the empty vector
      vector_new_codename[index_with_old_codename] <- dataset_unclean[index_with_old_codename, old_codename_o]
      
    }
    # Check if the values of the original codenames are in the new vector 
    # View(cbind(dataset_unclean[,c(old_codenames)], vector_new_codename) %>% unique(.))
    
    # Remove the column containing the original codenames for an occupational variable, since such information 
    # has been used
    long_subset_category_fix <- long_subset_category_fix %>%
      select(-codename_original) %>%
      mutate(SDDSRVYR = as.numeric(SDDSRVYR))
    # View(long_subset_category_fix)
    
    # There are new occupational codenames as one has a long description and the other has an abbreviated 
    # description.
    new_occup_codenames <- strsplit(new_codename_fix_f
                                    , ", ") %>%
      unlist(., use.names = FALSE)
    # print(new_occup_codenames)
    
    # The first codename will contain the long description of the occupational categories
    long_new_codename <- new_occup_codenames[1]
    
    # The second codename will contain the shorter description of the occupational categories
    short_new_codename <- new_occup_codenames[2]
    
    # Define a string to use as a codename to hold the unharmonized occupational categories
    old_occup_codename <- paste(long_new_codename
                                , "OLD"
                                , sep = "")
    # print(old_occup_codename)
    
    
    # If the a column name "collar_categories" is included, then define a vector of string to 
    # select the pertinent columns from the long formated datset of cleaning documentation
    if("collar_categories" %in% colnames(long_subset_category_fix) == TRUE)
    {
      colnames_interest_long_cleaning <- c("categories_num"
                                           , "categories_description"
                                           , "new_categories"
                                           , "new_categories_description"
                                           , "collar_categories"
                                           , "SDDSRVYR")
    } else {
      colnames_interest_long_cleaning <- c("categories_num"
                                           , "categories_description"
                                           , "new_categories"
                                           , "new_categories_description"
                                           , "SDDSRVYR")
    }
    
    dataset_unclean <- dataset_unclean %>%
      # Add the new column for where the original categories are placed in one vector instead of two
      mutate(categories_num = vector_new_codename) %>%
      # Merge in the numbers and descriptions of the harmonized categories by the number of the old
      # categories and the cycle year
      left_join(.
                , long_subset_category_fix[ , colnames_interest_long_cleaning] %>%
                  .[, colSums(is.na(.)) != nrow(.)]
                , by = c("categories_num"
                         , "SDDSRVYR")) %>%
      # Define a new column as the concatenation of the numbers and categories of the old categories
      mutate(old_category = paste(categories_num
                                  , categories_description
                                  , sep = " - ")) %>%
      # Define a new column as the concentanation of the numbers and categories of the harmonized categories
      mutate(new_category = paste(new_categories
                                  , new_categories_description
                                  , sep = " - "))
    
    # Determine the column indices pertaining to the newly defined columns with info on the
    # occupational categories
    col_index <- which(colnames(dataset_unclean) %in% c("new_categories_description"
                                                        , "new_category"
                                                        , "old_category"))
    
    # Redefine the column names to be new codenames for the shorter description, the old description
    # and the long description
    colnames(dataset_unclean)[col_index] <- c(short_new_codename
                                              , old_occup_codename
                                              , long_new_codename)
    
    # # View the different combinations of the selected columns to check if the harmonization was done correctly
    # View(dataset_unclean[,c(old_codenames
    #                       , "categories_num"
    #                       , "categories_description"
    #                       , "new_categories"
    #                       , old_occup_codename
    #                       , long_new_codename
    #                       , short_new_codename
    #                       , "SDDSRVYR")] %>% 
    #        unique(.))
    
    # Remove extraneous columns from the NHANES dataset as shorter description, old description, and longer 
    # description are in the dataset
    dataset_unclean <- dataset_unclean %>%
      select(-c(categories_num
                , categories_description
                , new_categories))
    # print(colnames(long_subset_category_fix))
    
    # # Print out different combinations of the shorter description, old description, and longer description
    # # to check for correct harmonization
    # View(dataset_unclean[,c(short_new_codename
    #                         , old_occup_codename
    #                         , long_new_codename
    #                         , "collar_categories"
    #                         , "SDDSRVYR")] %>% 
    #        unique(.))
    
    # If the the occupational variable pertains to job description, then ensure that the column name pertaining  
    # to collar categories is a codename  
    if("collar_categories" %in% colnames(long_subset_category_fix) == TRUE)
    {
      # Determine the new codename for the collar categories
      new_collar_codename <- long_subset_category_fix$new_codename_collar %>%
        unique(.) %>%
        unlist(., use.names = FALSE)
      # print(new_collar_codename)
      
      # Determine the column index of "collar_categories" so that we will replace it with the new codename
      index_collar_categories <- which(colnames(dataset_unclean) == "collar_categories")
      
      # Replace column name with the new codename
      colnames(dataset_unclean)[index_collar_categories] <- new_collar_codename
    }
    
  }
  
  # Due to using paste(), some values will be "NA - NA", since there is missing data. 
  # Ensure that anything with value as "NA - NA" is deem missing, so has the value NA
  dataset_unclean[dataset_unclean == "NA - NA"] <- NA
  
  # Deem the dataset as clean after harmonizing the occupational categories 
  dataset_clean <- dataset_unclean
  
  return(dataset_clean)
}