calculate_num_measurements_cycles <- function(x
                                              , df_nhanes)
{
  # print(str(df_nhanes))
  
  vector_cycles <- df_nhanes[,"SDDSRVYR"]
  
  SEQN_new <- df_nhanes[,"SEQN_new"]
  
  # print(colnames(x))
  
  df_cycles_labels <- data.frame(cycles = c(-1, seq(10))
                                 , cycle_labels = c("1988-1994"
                                                    , "1999-2000"
                                                    , "2001-2002"
                                                    , "2003-2004"
                                                    , "2005-2006"
                                                    , "2007-2008"
                                                    , "2009-2010"
                                                    , "2011-2012"
                                                    , "2013-2014"
                                                    , "2015-2016"
                                                    , "2017-2018")
                                 , stringsAsFactors = FALSE) 
  # View(df_cycles_labels)
  
  # num_participants_measurements <- sum(is.na(x) == FALSE) 
  # print(num_measurements)
  
  df_variable_cycle <- data.frame(SEQN_new = SEQN_new
                                  , variable_i = x
                                  , cycles = vector_cycles
                                  , stringsAsFactors = FALSE) %>%
    na.omit(.) %>%
    left_join(.
              , df_cycles_labels
              , by = "cycles")
  
  num_participants_measurements <- df_variable_cycle %>%
    pull(SEQN_new) %>%
    unique(.) %>%
    length(.)
  
  unique_cycles <- df_variable_cycle %>%
    pull(cycle_labels) %>%
    unique(.) %>%
    sort(.) %>%
    paste(.
          , collapse = ", ")
  # print(unique_cycles)
  
  df_stats <- data.frame(num_participants_measurements = num_participants_measurements
                         , unique_cycles = unique_cycles
                         , stringsAsFactors = FALSE)
  
  return(df_stats)
}