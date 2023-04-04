clean_duplicates_of_seqn_from_cycles <- function(unclean_dataset)
{
  library(tidyverse)
  
  df_cycles <- unclean_dataset %>%
    group_by(SEQN_new) %>%
    summarise(SDDSRVYR = min(SDDSRVYR)) %>%
    ungroup(.) %>%
    mutate(keep = TRUE)
  # View(df_cycles)
  
  unclean_dataset <- unclean_dataset %>%
    full_join(.
              , df_cycles
              , by = c("SEQN_new"
                       , "SDDSRVYR"))
  # View(unclean_dataset %>%
  #        select(SEQN, SEQN_new, SDDSRVYR, keep))
  
  dataset_cleaner <- unclean_dataset %>%
    filter(keep == TRUE) %>%
    select(-keep)
  
  return(dataset_cleaner)
}