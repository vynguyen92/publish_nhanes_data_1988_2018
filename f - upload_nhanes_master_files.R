upload_nhanes_master_files <- function(name_of_file)
{
  library(readxl)
  library(tidyverse)
  
  # Determine the name of all sheets in this master file
  sheets_name <- excel_sheets(name_of_file)[-1]
  # print(sheets_name)
  
  # sheets_name <- sheets_name[1]
  
  # Initialize an empty list to store the dataset containing the cleaning documentation
  list_of_master_files <- list()
  
  # Determine the number of documentation datasets
  num_sheets <- length(sheets_name)
  
  for(i in seq(num_sheets))
  {
    # Extract the name of a given sheet
    sheet_name_i <- sheets_name[i]
    print(sheet_name_i)
    
    if(sheet_name_i == "Response")
    {
      excel_sheet_i <- read_excel(name_of_file
                                  , sheet = sheet_name_i
                                  , col_types = c("text"
                                                  , "text"
                                                  , "text"
                                                  , "text"
                                                  , "text"
                                                  , "text"
                                                  , "text"
                                                  , "text"
                                                  , "numeric"
                                                  , "text"
                                                  , "numeric"
                                                  , "text"
                                                  , "text"
                                                  , "numeric"
                                                  , "text"
                                                  , "text"
                                                  , "numeric"
                                                  , "text"
                                                  , "text"))
      
    } else if(sheet_name_i == "Dietary") {
      
      excel_sheet_i <- read_excel(name_of_file
                                  , sheet = sheet_name_i
                                  , col_types = c("text"	
                                                  , "text"	
                                                  , "text"	
                                                  , "text"	
                                                  , "text"	
                                                  , "text"
                                                  , "text"
                                                  , "text"
                                                  , "numeric"	
                                                  , "text"	
                                                  , "numeric"	
                                                  , "text"
                                                  , "text"	
                                                  , "text"
                                                  , "text"	
                                                  , "text"	
                                                  , "numeric"
                                                  , "text"
                                                  , "text"
                                                  , "numeric"	
                                                  , "text"
                                                  , "numeric"	
                                                  , "text"))
      
    } else if(sheet_name_i == "Chemicals") {
      
      excel_sheet_i <- read_excel(name_of_file
                                  , sheet = sheet_name_i
                                  , col_types = c("text"	
                                                  , "text"	
                                                  , "text"	
                                                  , "text"	
                                                  , "text"	
                                                  , "text"	
                                                  , "text"
                                                  , "text"
                                                  , "text"	
                                                  , "text"
                                                  , "text"
                                                  , "numeric"
                                                  , "numeric"
                                                  , "numeric"
                                                  , "text"	
                                                  , "text"
                                                  , "text"	
                                                  , "text"
                                                  , "text"
                                                  , "text"	
                                                  , "text"	
                                                  , "text"
                                                  , "numeric"
                                                  , "numeric"
                                                  , "text"
                                                  , "numeric"
                                                  , "text"
                                                  , "text"
                                                  , "text"
                                                  , "text"
                                                  , "numeric"	))
      
      
    } else if(sheet_name_i == "Weights") {
      
      excel_sheet_i <- read_excel(name_of_file
                                  , sheet = sheet_name_i
                                  , col_types = c("text"	
                                                  , "text"
                                                  , "text"	
                                                  , "text"	
                                                  , "numeric"
                                                  , "text"	
                                                  , "text"
                                                  , "text"	
                                                  , "text"
                                                  , "text"))
      
      
    } else if(sheet_name_i == "Questionnaire") {
      
      excel_sheet_i <- read_excel(name_of_file
                                  , sheet = sheet_name_i
                                  , col_types = c("text"	
                                                  , "text"	
                                                  , "text"	
                                                  , "text"	
                                                  , "text"
                                                  , "text"
                                                  , "text"
                                                  , "text"
                                                  , "numeric"
                                                  , "text"	
                                                  , "numeric"
                                                  , "text"
                                                  , "text"	
                                                  , "numeric"
                                                  , "text"))
      
      
    } else {
      # Read in a documentation dataset using the name of the sheet
      excel_sheet_i <- read_excel(name_of_file
                                  , sheet = sheet_name_i)
    }
    
    
    # View(excel_sheet_i)
    
    # If the documentation dataset pertains to the chemical, then include two extra columns to indicate the 
    # chemical and comment codenames to use after harmonizing to have only one codename for one chemical or comment
    if(sheet_name_i == "Chemicals")
    {
      # Define a vector to contain the column name pertaining original codenames
      original_codenames <- c("chemical_codename"
                              , "comment_codename") 
      
      # Define a vector to contain the new column names
      new_correcting_codenames <- c("chemical_codename_use"
                                    , "comments_codename_use")
      
      # # Add a new column to contain the harmonized codenames
      # excel_sheet_i <- include_column_for_harmonized_codenames(original_codenames
      #                                                          , new_correcting_codenames
      #                                                          , excel_sheet_i)
      
      
      # # Ensure that the chemical codename and chemical codename to use are at the beginning
      # excel_sheet_i <- excel_sheet_i %>%
      #   relocate(chemical_codename_use
      #            , .after = chemical_codename)
      
      
      # View(excel_sheet_i %>%
      #        select(chemical_codename_use
      #               , comment_codename
      #               ,	corrected_comment_codename
      #               , comments_codename_use) %>%
      #        unique(.))
    } else if(grepl("Fix Category", sheet_name_i) == FALSE 
              & !(sheet_name_i %in% c("Weights", "Mortality", "Biomonitoring Equivalents"))) {
      
      # Define a vector to contain the column name pertaining original codenames
      original_codenames <- "variable_codename"
      
      # Define a vector to contain the new column names
      new_correcting_codenames <- c("variable_codename_use")
      
      # # Add a new column to contain the harmonized codenames
      # excel_sheet_i <- include_column_for_harmonized_codenames(original_codenames
      #                                                          , new_correcting_codenames
      #                                                          , excel_sheet_i)
      
      # # Ensure that the chemical codename and chemical codename to use are at the beginning
      # excel_sheet_i <- excel_sheet_i %>%
      #   relocate(variable_codename_use
      #            , .after = variable_codename)
    } else {
      
    }
    
    # Store each documentation dataset into the list 
    list_of_master_files[[sheet_name_i]] <- excel_sheet_i
  }
  
  return(list_of_master_files)
}
