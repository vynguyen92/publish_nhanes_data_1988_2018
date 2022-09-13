ensure_reasonable_comments <- function(dataset_new
                                       , df_doc_cleaning)
{
  subset_doc_cleaning <- df_doc_cleaning %>%
    filter(is.na(comment_codename_use) == FALSE) %>%
    filter(is.na(LOD) == FALSE)
  
  comments_codenames <- subset_doc_cleaning %>%
    pull(comment_codename_use) %>%
    unique(.)
  
  num_unique_comments <- length(comments_codenames)
  
  for(i in seq(num_unique_comments))
  {
    comment_codename_i <- comments_codenames[i]
    
    
    index_comments_wrong <- which(dataset_new[,comment_codename_i] > 2 
                                  | dataset_new[,comment_codename_i] < 0)
    
    unreasonable_comments <- is_empty(index_comments_wrong)
    
    if(unreasonable_comments == FALSE)
    {
      print(comment_codename_i)
    }
    
    dataset_new[index_comments_wrong,comment_codename_i] <- NA
  }
  
  dataset_clean <- dataset_new
  
  return(dataset_clean)
}