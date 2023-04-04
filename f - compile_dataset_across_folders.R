#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###########################  FUNCTION TO COMPILE NHANES DATASET ACROSS FOLDERS  ###############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function extracts the datasets for each cycle, merges all datasets within a cycle together, and 
#          merges all cycle-specific datasets into one merged dataset. There should be a single folder for a 
#          given cycle. 
#
# Inputs: dataset_directory - the working directory of the folder that contains the files of cycle-specific
#                          demographics dataset
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project is housed.
#
# Outputs: merged_nhanes_dataset - dataframe with the merged dataset with attributes labelled

compile_dataset_across_folders <- function(dataset_directory
                                           , current_directory)
{
  # Establish the working directory to be the folder that contains the folders of the cycle-specific response
  # datasets
  setwd(dataset_directory)
  
  # Obtain a vector of folder names, one for each cycle
  nhanes_dataset_by_cycle <- list.files()
  # print(nhanes_dataset_by_cycle)
  
  nhanes_dataset_by_cycle <- nhanes_dataset_by_cycle#[10]
  
  # Determine the number of cycles 
  num_cycles <- length(nhanes_dataset_by_cycle)
  
  # Initialize a list to store a dataframe of biomarker measurements for each cycle
  all_nhanes_datasets_by_cycle_labeled <- list()
  all_nhanes_datasets_by_cycle_removed <- list()
  
  # For each cycle in NHANES, go into a cycle-specific folder and extract the corresponding files to form a 
  # merged chemicals datasest for an ith cycle
  for(i in seq(num_cycles))
  {
    # Define the directory of the ith cycle-specific folder
    cycle_specific_nhanes_dataset_directory <- paste(dataset_directory
                                                     , nhanes_dataset_by_cycle[i]
                                                     , sep = "/")
    # print(cycle_specific_nhanes_dataset_directory)
    
    # Establish the working directory for the ith cycle-specific folder
    setwd(cycle_specific_nhanes_dataset_directory)
    
    # Determine a vector of file names in the ith cycle-specific folder
    files_names.xpt <- list.files()
    
    # Determine the number of files in the ith cycle-specific folder
    num_files_in_cycle_specific_folder <- length(files_names.xpt)
    
    for(j in seq(num_files_in_cycle_specific_folder))
    {
      # Determine the name of the jth file in the folder and replace the ".XPT" in the file name with ""
      file_name_j <- gsub(".XPT"
                          , ""
                          , files_names.xpt[j])
      # Message to know which chemical dataset is being extracted
      # print(file_name_j)
      
      # Store jth chemical dataset into temp_file
      temp_file <- nhanes(file_name_j)
      
      # Ensure that the data type of the respondent identifier is numeric and the same across all datasets
      # to help with the merging
      temp_file$SEQN <- as.numeric(temp_file$SEQN)
      
      # Ensure that the attribute of the respondent identifier is labelled.
      attr(temp_file$SEQN, "label") <- "Respondent sequence number"
      
      if(j == 1)
      {
        # Use the updated file name to extract the appropriate dataset from the nhanesA package
        # Store the first chemical dataset for the ith cycle into cycle_specific_datasest, so that subsequent
        # chemical datasets can be merged with the first one
        cycle_specific_dataset <- temp_file
        
      } else {
        
        # Merge the jth chemical dataset with the previous chemicals datset by SEQN
        cycle_specific_dataset <- full_join(cycle_specific_dataset
                                            , temp_file
                                            , by = "SEQN")
      }
    }
    
    if(TRUE %in% grepl(".x$|.y$", colnames(cycle_specific_dataset)))
    {
      # Call a function to resolve the duplicates
      cycle_specific_dataset <- resolve_duplicates(cycle_specific_dataset)
    }
    
    # Store the merged chemical datset for the ith cycle into a list
    all_nhanes_datasets_by_cycle_labeled[[i]] <- data.frame(cycle_specific_dataset) %>%
      mutate(SDDSRVYR = rep(i, nrow(.)))
    
    # Remove attributes from cycle-specific dataset to make the merging easier 
    all_nhanes_datasets_by_cycle_removed[[i]] <- remove_all_labels(all_nhanes_datasets_by_cycle_labeled[[i]])
    
  }
  
  # Determine the number of cycles or technically, determine the number of dataframes stored in this list
  num_elements_in_list <- length(lengths(all_nhanes_datasets_by_cycle_removed))
  # print(num_elements_in_list)
  
  # Define a function to merge the datasets by column names
  joining_by_colnames <- function(x, y) full_join(x, y, by = NULL)
  
  # Merge all the datasets together 
  dataset_merged <- all_nhanes_datasets_by_cycle_removed %>%
    reduce(joining_by_colnames)
  
  # Reinstate the attributes into the merged dataset of demographics
  merged_nhanes_dataset_final <- copy_labels_into_nhanes(list_nhanes_datasets = all_nhanes_datasets_by_cycle_labeled
                                                         , merged_dataset = dataset_merged)
  
  # Set the working directory back
  setwd(current_directory)
  
  # Return the merged demographics dataset
  return(merged_nhanes_dataset_final)
}