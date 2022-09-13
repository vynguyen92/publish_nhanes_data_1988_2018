exclude_extraneous_indicators <- function(dataset_comments
                                          , df_doc_cleaning)
{
  subset_doc_cleaning <- df_doc_cleaning %>%
    filter(is.na(comment_codename_use) == FALSE)
  
  comments_codenames <- subset_doc_cleaning %>%
    pull(comment_codename_use) %>%
    unique(.)
  
  
  num_unique_comments <- length(comments_codenames)
  
  for(i in seq(num_unique_comments))
  {
    comment_codename_i <- comments_codenames[i]
    
    # comment_codename_i <- "URDUCDLC"
    
    chemical_codename_i <- subset_doc_cleaning %>%
      filter(comment_codename_use == comment_codename_i) %>%
      pull(variable_codename_use) %>%
      unique(.)
   
    # chemical_codename_i <- "URXUCD"
    
    if(length(chemical_codename_i) > 1)
    {
      # print(comment_codename_i)
      
      index_ending_la <- which(grepl("LA$|L$",chemical_codename_i) == TRUE)
      chemical_codename_i <- chemical_codename_i[index_ending_la]
      # print(chemical_codename_i)
    }
    
    
    subset_chemical_comment_i <- dataset_comments %>%
      select(all_of(comment_codename_i)
             , all_of(chemical_codename_i)) 
    
    colnames(subset_chemical_comment_i)[1] <- "comments"
    
    colnames(subset_chemical_comment_i)[2] <- "measurements"
    
    subset_chemical_comment_i <- subset_chemical_comment_i %>%
      mutate(missing = ifelse(is.na(measurements) == TRUE
                              , 1
                              , 0)) %>%
      mutate(comments = ifelse(missing == 1
                               , NA
                               , comments))
    # View(subset_chemical_comment_i %>% unique(.))
    
    dataset_comments[,comment_codename_i] <- subset_chemical_comment_i$comments
    

    
    num_comments <- sum(!is.na(dataset_comments[,comment_codename_i]))
    # print(num_comments)

    num_measurements <- sum(!is.na(dataset_comments[,chemical_codename_i]))
    # print(num_measurements)
    
    if(num_comments != num_measurements)
    {
     
      
      subset_nhanes_comments_chem_i <- dataset_comments %>%
        select(all_of(comment_codename_i)
               , all_of(chemical_codename_i)
               , SDDSRVYR) %>%
        unique(.) %>%
        filter(is.na(eval(parse(text = comment_codename_i))) == TRUE) %>%
        filter(is.na(eval(parse(text = chemical_codename_i))) == FALSE)
      # View(subset_nhanes_comments_chem_i)
      
      cycle_missing_comments <- subset_nhanes_comments_chem_i %>%
        pull(SDDSRVYR) %>%
        unique(.)
     
      
      cycles_comments_doc_cleaning_i <- df_doc_cleaning %>%
        filter(comment_codename_use == comment_codename_i) %>%
        pull(SDDSRVYR) %>%
        unique(.)
      
      
      intersect_cycles <- intersect(cycle_missing_comments
                                    , cycles_comments_doc_cleaning_i)
      
      no_intersecting_cycles <- is_empty(intersect_cycles)
      
      if(no_intersecting_cycles == FALSE)
      {
        print(comment_codename_i)
        print(chemical_codename_i)
        # print(num_comments)
        # print(num_measurements)
        # 
        print(cycle_missing_comments)
        print(cycles_comments_doc_cleaning_i)
        print(intersect_cycles)
      }
      
    }
      
  }
  
  dataset_cleaner <- dataset_comments
  
  return(dataset_cleaner)
}