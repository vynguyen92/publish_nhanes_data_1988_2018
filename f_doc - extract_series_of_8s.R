extract_series_of_8 <- function(dataset_unclean
                                , name_dataset)
{
  library(tidyverse)
  
  subset_nhanes_iii <- dataset_unclean %>%
    filter(SDDSRVYR == -1)
  
  obtain_eights <- function(x)
  {
    pattern <- "^.*8{2,}.*$"
    matches <- grep(pattern, as.character(x), value = TRUE)
    
    matches <- as.numeric(matches) %>%
      unique(.)
    
    # print(matches)
    # 
    # print(length(matches))
    # 
    if(length(matches) == 0)
    {
      matches <- NA
    } 
    
    df_matches <- data.frame(eights = matches)
    return(df_matches)
  }
  
  
  df_eights <- subset_nhanes_iii %>%
    select_if(~!all(is.na(.))) %>%
    select(-c(SEQN, SEQN_new)) %>%
    map_df(.
           , obtain_eights
           , .id = "column_name")
  
  # View(df_eights)
  
  name_file_df_eights <- paste("series_8_updated_"
                               , name_dataset
                               , ".csv"
                               , sep = "")
  
  write.csv(df_eights, name_file_df_eights)
}