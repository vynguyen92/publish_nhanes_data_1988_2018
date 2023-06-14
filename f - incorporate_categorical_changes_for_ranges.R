incorporate_categorical_changes_for_ranges <- function(long_fix_cat_documentation
                                                       , subset_documentation)
{
  # View(long_fix_cat_documentation)
  
  # View(subset_documentation)
  
  new_codename <- subset_documentation %>%
    pull(new_codename) %>%
    unique(.)
  
  long_subset_documentation <- subset_documentation  %>%
    pivot_longer(cols = grep("^cycle", colnames(.))
                 , names_to = "year"
                 , values_to = "value") %>%
    drop_na(value) %>%
    mutate(SDDSRVYR = as.numeric(gsub("cycle_"
                                      , ""
                                      , year))) %>%
    select(categories_num, new_categories, new_codename, SDDSRVYR) #%>%
    # mutate(new_categories = as.numeric(new_categories))
  # View(long_subset_documentation)
  
  unique_ranges <- long_subset_documentation %>%
    pull(categories_num) %>%
    unique(.)
  # print(unique_ranges)

  num_categories_ranges <- length(unique_ranges)

  list_fix_categories <- list()
  
  for(i in seq(num_categories_ranges))
  {
    ranges_i <- unique_ranges[i]
    # print(ranges_i)

    split_range <- strsplit(ranges_i
                            , "-") %>%
      unlist(.) %>%
      as.numeric(.)
    # print(split_range)

    beginning <- split_range[1]

    ending <- split_range[2]

    sequence_old_categories_i <- seq(from = beginning
                                     , to = ending
                                     , by = 1)
    # print(sequence_old_categories_i)

    num_categories <- length(sequence_old_categories_i)

    new_categories_i <- long_subset_documentation %>%
      filter(categories_num == ranges_i) %>%
      pull(new_categories) %>%
      # as.numeric(.) %>%
      unique(.)
    # print(new_categories_i)
    
    if(grepl("-", new_categories_i) == TRUE)
    {
      # print("yes")
      
      split_range_new_cat <- strsplit(new_categories_i
                                      , "-") %>%
        unlist(.) %>%
        as.numeric(.) 
      # print(split_range_new_cat)
        
      beginning_new_cat <- split_range_new_cat[1]
      
      ending_new_cat <- split_range_new_cat[2]
      
      new_categories_i <- seq(from = beginning_new_cat
                                       , to = ending_new_cat
                                       , by = 1)
      
    } else {
      new_categories_i <- as.numeric(new_categories_i)
    }
    # print(new_categories_i)

    cycle_i <- long_subset_documentation %>%
      filter(categories_num == ranges_i) %>%
      pull(SDDSRVYR) %>%
      as.numeric(.)
    # print(cycle_i)
    
    num_cycles <- length(cycle_i)

    for(j in seq(num_cycles))
    {
      cycle_j <- cycle_i[j]
      # print(cycle_j)
      
      # print(sequence_old_categories_i)
      
      # print(new_categories_i)
      
      subset_partial_cycle_j <- data.frame(categories_num = sequence_old_categories_i
                                           , new_categories = new_categories_i
                                           , new_codename = new_codename
                                           , SDDSRVYR = cycle_j)
      # print(subset_partial_cycle_j)
      
      list_fix_categories[[paste(i, j)]] <- subset_partial_cycle_j
    }
    
  }
  # View(list_fix_categories)
  
  # Define a function to merge the datasets by column names
  joining_by_colnames <- function(x, y) full_join(x
                                                  , y
                                                  , by = colnames(x))
  
  long_dataset_fix_partial <- list_fix_categories %>%
    reduce(joining_by_colnames)
  
  # View(long_dataset_fix_partial)
  
  # View(long_dataset_fix_partial %>%
  #   select(categories_num
  #          , new_categories) %>%
  #   unique(.))

  long_fix_cat_documentation <- long_fix_cat_documentation %>%
    full_join(.
              , long_dataset_fix_partial
              , by = colnames(.))
  # View(long_fix_cat_documentation)

  return(long_fix_cat_documentation)
}