clean_duplicates_of_seqn_from_cycles <- function(list_unclean,
                       unclean_response)
{
  library(tidyverse)
  
  df_cycles <- unclean_response %>%
    group_by(SEQN_new) %>%
    summarise(SDDSRVYR = min(SDDSRVYR)) %>%
    ungroup(.) %>%
    mutate(keep = TRUE)
  # View(df_cycles)
  
  unclean_response <- unclean_response %>%
    full_join(.
              , df_cycles
              , by = c("SEQN_new"
                       , "SDDSRVYR"))
  
  # num_cycles <- length(list_unclean)
  # # print(num_cycles)
  # 
  # names_datasets <- names(list_unclean)
  # 
  # unclean_response <- unclean_response #%>%
  #   # mutate(keep = rep(NA
  #   #                   , times = nrow(.)))
  
  # for(i in c(3,4))
  # {
  #   
  #   cycle_name_i <- names_datasets[i]
  #   print(cycle_name_i)
  #   
  #   cycle_i <- strsplit(cycle_name_i
  #                       , split = " ") %>%
  #     unlist(.) %>%
  #     .[2] %>%
  #     as.numeric(.)
  #   print(cycle_i)
  #  
  #   subset_response <- list_unclean[[i]]
  #   
  #   df_SEQN_cycle <- subset_response %>%
  #     select(SEQN, SEQN_new, SDDSRVYR) %>%
  #     mutate(cycle = SDDSRVYR) %>%
  #     unique(.)
  #   # View(df_SEQN_cycle)
  #   
  #   unclean_response <- unclean_response %>%
  #     left_join(.
  #               , df_SEQN_cycle
  #               , by = c("SEQN"
  #                        , "SEQN_new"
  #                        , "SDDSRVYR")) 
  #     
  # 
  #   index_keep <- which(unclean_response$SDDSRVYR == unclean_response$cycle)
  #   print(index_keep)
  #   
  #   unclean_response <- unclean_response %>%
  #     select(-cycle)
  #   
  #   # View(unclean_response %>% 
  #   #        select(SEQN, SEQN_new, SDDSRVYR, cycle, keep))
  #   
  #   
  #   # subset_response_i <- unclean_response %>%
  #   #   filter(cycle == cycle_i)
  #   # View(subset_response_i)
  #   
  #   # if(i == 1)
  #   # {
  #   #   df_cycle <- df_SEQN_cycle
  #   # } else {
  #   #   df_cycle <- df_cycle %>%
  #   #     full_join(.
  #   #               , df_SEQN_cycle
  #   #               , by = colnames(.))
  #   # }
  # }
  View(unclean_response %>%
         select(SEQN, SEQN_new, SDDSRVYR, keep))
}