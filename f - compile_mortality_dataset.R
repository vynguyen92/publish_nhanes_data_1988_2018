#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###########################   FUNCTION TO COMPILE THE NHANES MORTALITY DATASET  ###############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function extracts the individual mortality dataset for each cycle and merge into one combined 
#          mortality dataset
#
# Inputs: dataset_directory - the directory of the folder containing the mortality datasets
#         current_directory - the directory of the folder where the function and main scripts of the 
#                             project is housed.
#
# Outputs: merged_nhanes_dataset - a dataframe with the merged mortality dataset 

compile_mortality_dataset <- function(dataset_directory
                                      , current_directory)
{
  library(readxl)
  library(readr)
  library(tidyverse)
  
  # Set the directory to the folder containing the individual mortality datasets
  setwd(dataset_directory)
  
  # Read in the excel file containing information for each variable in the mortality dataset
  dataset_mortality_dictionary <- read_excel("NHANES - Mortality Variable Descriptions.xlsx")
  # View(dataset_mortality_dictionary)

  # Obtain all file names of the individual mortality datasets
  mortality_dataset_by_cycle <- list.files(pattern = ".dat$")
  
  # mortality_dataset_by_cycle <- "NHANES_III_MORT_2019_PUBLIC.dat"

  # Determine the number of individual mortality datasets available, and this is the total number of cycles
  # with mortality data
  num_cycles <- length(mortality_dataset_by_cycle)

  # Perform this for each individual mortality dataset
  for(j in seq(num_cycles))
  {
    # Extract the file name of a given individual mortality dataset
    filename_j <- mortality_dataset_by_cycle[j]
    print(filename_j)

    # Read in the dataset and format the column vectors
    mortality_df_j <- read_fwf(file = filename_j,
                               col_types = "iiiiiiiiddii",
                               fwf_cols(SEQN = c(1,14),
                                        ELIGSTAT = c(15,15),
                                        MORTSTAT = c(16,16),
                                        UCOD_LEADING = c(17,19),
                                        DIABETES = c(20,20),
                                        HYPERTEN = c(21,21),
                                        DODQTR = c(22,22),
                                        DODYEAR = c(23,26),
                                        WGT_NEW = c(27,34),
                                        SA_WGT_NEW = c(35,42),
                                        PERMTH_INT = c(43,45),
                                        PERMTH_EXM = c(46,48)
                               ),
                               na = ".") 
    
    if(filename_j != "NHANES_III_MORT_2019_PUBLIC.dat")
    {
      mortality_df_j <- mortality_df_j %>%
        mutate(SEQN_new = paste("C-"
                                , SEQN
                                , sep = "")) %>%
        mutate(SDDSRVYR = j)
      
    } else {
      mortality_df_j <- mortality_df_j %>%
        mutate(SEQN_new = paste("I-"
                                , SEQN
                                , sep = "")) %>%
        mutate(SDDSRVYR = -1)
    }

    # Combine the dataset across the cycles together
    if(j == 1) {

      merged_nhanes_dataset <- mortality_df_j

    } else {

      merged_nhanes_dataset <- full_join(merged_nhanes_dataset
                                         , mortality_df_j
                                         , by = NULL)

    }

  }

  # Convert the dataset into a dataframe to help with labelling the attributes
  merged_nhanes_dataset <- data.frame(merged_nhanes_dataset)

  # Determine the total number of variables in the mortality dataset
  num_variables_mortality <- ncol(merged_nhanes_dataset)

  # Assign the attribute of each variable with the name of the variable
  for(i in seq(num_variables_mortality))
  {
    # Determine the variable codename
    variable_codename_i <- colnames(merged_nhanes_dataset)[i]

    # Determine the row index of the variable
    index_variable_i <- which(dataset_mortality_dictionary$Variable == variable_codename_i)

    # Extract the label of the variable
    variable_name_i <- as.character(dataset_mortality_dictionary[index_variable_i,"Label"])

    if(variable_codename_i == "SEQN")
    {
      merged_nhanes_dataset[,variable_codename_i] <- as.numeric(merged_nhanes_dataset[,variable_codename_i]) %>%
        as.integer(.)
    }

    # Include the label of the variable as its attribute
    attr(merged_nhanes_dataset[,variable_codename_i], "label") <- variable_name_i
  }
  
  attr(merged_nhanes_dataset[,"SEQN_new"], "label") <- "Respondent sequence number"
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  
  return(merged_nhanes_dataset)
}
