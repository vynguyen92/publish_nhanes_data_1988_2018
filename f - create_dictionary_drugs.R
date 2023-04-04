create_dictionary_drugs <- function(name_of_dataset)
{
  library("tidyverse")
  library("nhanesA")
  
  df_dictionary <- nhanes(name_of_dataset) #%>%
    # mutate(interest_to_ferrero = ifelse(RXDDCI1B %in% c(19, 55)
    #                                     , "yes"
    #                                     , NA))
  
  return(df_dictionary)
}