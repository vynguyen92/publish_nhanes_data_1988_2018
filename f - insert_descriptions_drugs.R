insert_descriptions_drugs <- function(data_unclean)
{
  library(tidyverse)
  library(sjlabelled)
  library(nhanesA)
  
  df_descriptions <- nhanes("RXQ_DRUG")
  
  df_descriptions$RXDDRGID <- remove_all_labels(df_descriptions$RXDDRGID)
  
  
  data_cleaner <- left_join(data_unclean %>%
                              select(-RXDDRUG)
                            , df_descriptions
                            , by = "RXDDRGID")
  
  return(data_cleaner)
}