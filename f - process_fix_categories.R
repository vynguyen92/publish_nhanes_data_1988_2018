process_fix_categories <- function(df_fix_categories)
{
  # View(df_fix_categories)
  
  df_fix_categories <- df_fix_categories %>%
    mutate(categories_num = gsub(" to "
                                 , "-"
                                 , categories_num)) %>%
    mutate(new_categories = gsub(" to "
                                 , "-"
                                 , new_categories))
  # View(df_fix_categories)
  # print(nrow(df_fix_categories))
  
  df_fix_not_ranges <- df_fix_categories %>%
    filter(categories_description != "Range of Values") 
  # print(nrow(df_fix_not_ranges))
  
  df_fix_ranges_16 <- df_fix_categories %>%
    filter(categories_description == "Range of Values") %>%
    filter(grepl("16;"
                 , new_categories) == TRUE)
  # print(nrow(df_fix_ranges_16))
  
  df_fix_ranges_not_16 <- df_fix_categories %>%
    filter(categories_description == "Range of Values") %>%
    filter(grepl("16;"
                 , new_categories) == FALSE)
 
  # print(nrow(df_fix_ranges_not_16))

  list_fix_ranges_16 <- list()
    
  num_rows <- nrow(df_fix_ranges_16)

  counter_16 <- 0
  
  counter_not_16 <- 0
  
  for(i in seq(num_rows)#[c(1,44,47)]
      )
  {
    df_fix_ranges_i <- df_fix_ranges_16[i,]
    # print(df_fix_ranges_i)

    # sixteen_in_new_categories <- grepl("16;"
    #                                    , df_fix_ranges_i$new_categories)
    # 
    # if(sixteen_in_new_categories == TRUE)
    # {
      ranges_text_i <- df_fix_ranges_i %>%
        pull(categories_num)
      # print(ranges_text_i)

      ranges_num_i <- strsplit(ranges_text_i
                               , "-") %>%
        unlist(.) %>%
        as.numeric(.)
      # print(ranges_num_i)

      min_range <- ranges_num_i[1]
      # print(min_range)

      max_range <- ranges_num_i[2]

      if(16 >= min_range & 16 < max_range)
      {
        range_younger <- paste(min_range
                               , 16
                               , sep = "-")
        # print(range_younger)
        
        range_older <- paste(17
                             , max_range
                             , sep = "-")
        # print(range_older)
        
        df_fix_ranges_i_new <- rbind(df_fix_ranges_i
                                     , df_fix_ranges_i) %>%
          rbind(.
                , df_fix_ranges_i)
        # print(df_fix_ranges_i_new)
        
        df_fix_ranges_i_new[2,"categories_num"] <- range_younger
        df_fix_ranges_i_new[3,"categories_num"] <- range_older
        
        df_fix_ranges_i_new[2,"categories_description"] <- paste(range_younger
                                                                 , "years old")
        df_fix_ranges_i_new[2,"new_categories_description"] <- "16 years or younger"
        df_fix_ranges_i_new[3,c("categories_description"
                                , "new_categories_description")] <- paste(range_older
                                                                          , "years old")
        
        df_fix_ranges_i_new[2,"new_categories"] <- "16"
        df_fix_ranges_i_new[3,"new_categories"] <- range_older
        
        df_fix_ranges_i_new <- df_fix_ranges_i_new %>%
          mutate(categories_num = ifelse(categories_num == "16-16"
                                         , "16"
                                         , categories_num))
        
        counter_16 <- counter_16 + 1
        
      } else {
        df_fix_ranges_i_new <- df_fix_ranges_i %>%
          mutate(categories_description = paste(categories_num
                                                , "years old")) %>%
          mutate(new_categories = categories_num) %>%
          mutate(new_categories_description = categories_description)
        
        # print(i)
        # print("yes")
        
        counter_not_16 <- counter_not_16 + 1
        
      }
      
      # print(df_fix_ranges_i_new)

      list_fix_ranges_16[[i]] <- df_fix_ranges_i_new
      
    # }
  }
  
  # Define a function to merge the datasets by column names
  joining_by_colnames <- function(x, y) full_join(x
                                                  , y
                                                  , by = NULL)
  
  # merged_nhanes_dataset_final <- list_all_datasets %>%
  #   reduce(joining_by_colnames)
  
  df_fix_ranges_16_new <- list_fix_ranges_16 %>%
    reduce(joining_by_colnames)
  # View(df_fix_ranges_16_new)
  
  # print(nrow(df_fix_ranges_16_new))
  
  # print(counter_16)
  
  # print(counter_not_16)
  
  df_fix_categories_new <- df_fix_not_ranges %>%
    full_join(.
              , df_fix_ranges_not_16
              , by = colnames(.)) %>%
    full_join(.
              , df_fix_ranges_16_new
              , by = colnames(.))
  
  # print(nrow(df_fix_not_ranges))
  # print(nrow(df_fix_ranges_not_16))
  # print(nrow(df_fix_categories_new))
  # View(df_fix_categories_new)
  
  write_csv(df_fix_categories_new
            , "Questionnaire Fix Category updated.csv")
  
}