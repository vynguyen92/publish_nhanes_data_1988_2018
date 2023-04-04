dumb_compile_demographics_datasets <- function(cleaning_documentation
                                               , current_directory)
{
  library(nhanesA)
  library(haven)
  library(readxl)
  library(readr)
  library(tidyverse)
  library(labelled)
  
  nhanes_iii_directory <- paste(current_directory
                                , "/NHANES III"
                                , sep = "")
  
  setwd(nhanes_iii_directory)
  
  nhanes_iii_positions <- read_excel("NHANES III - Positions of variables.xlsx") %>%
    mutate(width = end - start + 1) %>%
    mutate(width = ifelse(is.na(width) == TRUE
                          , 1
                          , width))
  # View(nhanes_iii_positions)
  
  # Determine a vector of file names in the ith cycle-specific folder
  files_names.xpt <- list.files()
  
  vector_files_nhanes_iii <- c("ADULT"
                               , "YOUTH")
  
  num_files_nhanes_iii <- length(vector_files_nhanes_iii)
  
  for(j in seq(num_files_nhanes_iii))
  {
    file_nhanes_iii_j <- vector_files_nhanes_iii[j]
    print(file_nhanes_iii_j)
    
    subset_nhanes_iii_j <- nhanes_iii_positions %>%
      filter(file_name == file_nhanes_iii_j)
    # print(subset_nhanes_iii_j)
    
    num_variables <- subset_nhanes_iii_j %>%
      pull(variable_codename) %>%
      length(.)
    
    column_types <- paste(subset_nhanes_iii_j$data_type
                          , collapse = "")
    
    column_width <- subset_nhanes_iii_j$width %>%
      as.numeric(.)

    file_name_j_updated <- paste(file_nhanes_iii_j
                                 , "\\."
                                 , sep = "")
    
    index_file_name_j <- which(grepl(tolower(file_name_j_updated)
                                     , tolower(files_names.xpt)))
    # print(index_file_name_j)
    
    file_name_j_use <- files_names.xpt[index_file_name_j]
    # print(file_name_j_use)
    
    nhanes_iii <- read_fwf(file = file_name_j_use,
                        col_types = column_types,
                        col_positions = fwf_widths(column_width
                                                   , col_names = subset_nhanes_iii_j$variable_codename),
                        na = ".") %>%
      mutate(SDDSRVYR = -1) %>%
      mutate(SEQN_new = paste("I-"
                              , SEQN
                              , sep = ""))
    print(str(nhanes_iii))
    
    if(j == 1)
    {
      nhanes_iii_merged <- nhanes_iii
    } else {
      nhanes_iii_merged <- full_join(nhanes_iii_merged
                                     , nhanes_iii
                                     , by = NULL)
    }
  }
  
  # View(nhanes_iii_merged)
  # print(nrow(nhanes_iii_merged))
  # print(nhanes_iii_merged %>% pull(SEQN) %>% unique(.) %>% length(.))
  
  
  file_names <- cleaning_documentation %>%
    pull(file_name) %>%
    unique(.)
  # print(file_names)

  num_files <- length(file_names)

  list_demographics <- list()

  for(i in seq(num_files))
  {
    file_i <- file_names[i]
    print(file_i)

    temp_df <- nhanes(file_i)

    temp_df <- temp_df %>%
      select(SEQN
             , SDDSRVYR
             , RIDAGEYR
             , RIAGENDR
             , RIDRETH1
             , INDFMPIR) %>%
      mutate(SEQN_new = paste("C-"
                              , SEQN
                              , sep = "")) %>%
      unlabel(.)

    list_demographics[[i]] <- temp_df
  }
  # Define a function to merge the datasets by column names
  joining_by_colnames <- function(x, y) full_join(x
                                                  , y
                                                  , by = NULL)

  merged_nhanes_dataset_final <- list_demographics %>%
    reduce(joining_by_colnames)

  merged_nhanes_dataset_final <- joining_by_colnames(nhanes_iii_merged
                                                     , merged_nhanes_dataset_final) %>%
    select(SEQN
           , SEQN_new
           , SDDSRVYR
           , RIDAGEYR
           , RIAGENDR
           , RIDRETH1
           , INDFMPIR
           , DMARETHN
           , DMARACER
           , DMAETHNR
           , HSSEX
           , HSAGEIR
           , DMPPIR)

  setwd(current_directory)

  return(merged_nhanes_dataset_final)
}