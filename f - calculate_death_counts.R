calculate_death_counts <- function(mortality_dataset)
{
  library(tidyverse)
  
  df_mortality_status <- mortality_dataset %>%
    filter(is.na(MORTSTAT) == FALSE) %>%
    group_by(MORTSTAT) %>%
    summarise(count = n()) %>%
    ungroup()
  View(df_mortality_status)
  
  df_mortality_status_year <- mortality_dataset %>%
    filter(is.na(MORTSTAT) == FALSE) %>%
    group_by(MORTSTAT, study_year) %>%
    summarise(count = n()) %>%
    ungroup()
  View(df_mortality_status_year)
  
  df_mortality_ucod <- mortality_dataset %>%
    filter(is.na(MORTSTAT) == FALSE) %>%
    group_by(UCOD_LEADING) %>%
    summarise(count = n()) %>%
    ungroup()
  View(df_mortality_ucod)
}