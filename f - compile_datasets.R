compile_datasets <- function(cleaning_documentation
                             , current_directory
                             , name_dataset)
{
  library(nhanesA)
  library(haven)
  library(readxl)
  library(readr)
  library(tidyverse)
  library(labelled)
  library(sjlabelled)
  
  # Obtain a vector of folder names, one for each cycle
  nhanes_dataset_by_cycle <- list.files()
  
  cycles_nhanes <- cleaning_documentation %>%
    pull(SDDSRVYR) %>%
    unique(.) %>%
    sort(.)
  # print(cycles_nhanes)
  
  num_cycles <- length(cycles_nhanes)
  print(num_cycles)
  
  list_datasets_per_cycle <- list()
  list_all_datasets <- list()


  for(i in seq(num_cycles))
  {
    cycle_i <- cycles_nhanes[i]
    print(cycle_i)

    subset_cleaning_doc_i <- cleaning_documentation %>%
      filter(SDDSRVYR == cycle_i)
    # print(subset_cleaning_doc_i)

    if("problem_with_file" %in% colnames(subset_cleaning_doc_i))
    {

      subset_cleaning_doc_i <- subset_cleaning_doc_i %>%
        filter(is.na(problem_with_file) == TRUE)

    } else {
      subset_cleaning_doc_i <- subset_cleaning_doc_i
    }
    # View(subset_cleaning_doc_i)

    files_names <- subset_cleaning_doc_i %>%
      drop_na(file_name) %>%
      pull(file_name) %>%
      unique(.) #%>%
    # .[27]
    # print(files_names)

    num_files <- length(files_names)
    # print(num_files)

    # print("statistic_replicates" %in% colnames(subset_cleaning_doc_i))

    if("statistic_replicates" %in% colnames(subset_cleaning_doc_i) == TRUE)
    {
      codenames_include <- subset_cleaning_doc_i %>%
        filter(!(statistic_replicates %in% c("rowMeans"
                                             , "rowSums"
                                             , "/"
                                             , "calculate_estimated_gfr"))) %>%
        pull(variable_codename) %>%
        unique(.)

    } else {

      if(name_dataset == "Weights")
      {
        codenames_include <- subset_cleaning_doc_i %>%
          pull(weight_codename) %>%
          unique(.) %>%
          strsplit(.
                   , split = ", ") %>%
          unlist(.) %>%
          unique(.)

      } else if(name_dataset == "Occupation") {

        codenames_include <- subset_cleaning_doc_i %>%
          filter(is.na(file_name) == FALSE) %>%
          pull(variable_codename) %>%
          unique(.)

      } else {

        # if(cycle_i == -1)
        # {
        #   codenames_include <- subset_cleaning_doc_i %>%
        #     filter(is.na(file_name) == FALSE) %>%
        #     pull(variable_codename) %>%
        #     unique(.)
        #
        # } else {
          codenames_include <- subset_cleaning_doc_i %>%
            pull(variable_codename) %>%
            unique(.)
        # }


      }
      # print(codenames_include)


    }

    if(name_dataset == "Chemicals")
    {
      comments_codename <- subset_cleaning_doc_i %>%
        drop_na(comment_codename) %>%
        pull(comment_codename) %>%
        unique(.)
      # print(comments_codename)

      codenames_include <- codenames_include %>%
        append(., comments_codename)

    }
    # print(codenames_include)

    if(cycle_i == -1)
    {
      nhanes_iii_directory <- paste(current_directory
                                    , "/NHANES III"
                                    , sep = "")

      setwd(nhanes_iii_directory)

      # Determine a vector of file names in the ith cycle-specific folder
      files_names.xpt <- list.files()
      # print(files_names.xpt)
    }

    list_datasets_per_cycle <- list()

    for(j in seq(num_files))
    {
      file_name_j <- files_names[j]
      print(file_name_j)

      if(cycle_i == -1)
      {
        file_name_j_updated <- paste(file_name_j
                                     , "\\."
                                     , sep = "")
        # print(file_name_j_updated)

        index_file_name_j <- which(grepl(tolower(file_name_j_updated)
                                         , tolower(files_names.xpt)))
        # print(index_file_name_j)

        file_name_j_use <- files_names.xpt[index_file_name_j]
        # print(file_name_j_use)

        file_type_j <- strsplit(file_name_j_use
                                , split = "\\.") %>%
          unlist(.) %>%
          .[2]
        # print(file_type_j)

        if(file_type_j == "xpt")
        {
          temp_df <- read_xpt(file_name_j_use)
          # print(str(temp_df))

        } else if(file_type_j == "dat") {

          nhanes_iii_positions <- read_excel("NHANES III - Positions of variables.xlsx") %>%
            mutate(width = end - start + 1) %>%
            mutate(width = ifelse(is.na(width) == TRUE
                                  , 1
                                  , width))
          # View(nhanes_iii_positions)

          subset_nhanes_iii_j <- nhanes_iii_positions %>%
            filter(file_name == file_name_j)
          # View(subset_nhanes_iii_j)

          num_variables <- subset_nhanes_iii_j %>%
            pull(variable_codename) %>%
            length(.)

          column_types <- paste(subset_nhanes_iii_j$data_type
                                , collapse = "")
          # print(column_types)

          column_width <- subset_nhanes_iii_j$width %>%
            as.numeric(.)
          # print(column_width)



          temp_df <- read_fwf(file = file_name_j_use,
                              col_types = column_types,
                              col_positions = fwf_widths(column_width
                                                         , col_names = subset_nhanes_iii_j$variable_codename),
                              na = ".")
          # View(temp_df)



          # print(str(temp_df$DEPSTLC1))

        }




      } else {
        # Store jth dataset into temp_file
        temp_df <- nhanes(file_name_j)

        # print(str(temp_df))
      }

      # print(which(colnames(temp_df) == "original_file"))
      # print(unique(temp_df$original_file))

      list_datasets_per_cycle[[j]] <- temp_df %>%
        remove_var_label(.)

    }
    # View(list_datasets_per_cycle)

    # joining_by_SEQN <- function(x, y)
    # {
    #   variables_common <- intersect(colnames(x)
    #                                 , colnames(y))
    #
    #   print(variables_common)
    #
    #   merged_dataset <- full_join(x
    #                               , y
    #                               , by = variables_common)
    #
    #   return(merged_dataset)
    # }

    joining_by_SEQN <- function(x, y) full_join(x, y, by = "SEQN")

    # print(length(list_datasets_per_cycle))
    # print(names(list_datasets_per_cycle))

    if(length(list_datasets_per_cycle) == 1)
    {
      df_merged_for_cycle_i <- list_datasets_per_cycle[[1]]
    } else {
      df_merged_for_cycle_i <- list_datasets_per_cycle %>%
        reduce(joining_by_SEQN)
    }

    # View(df_merged_for_cycle_i)
    # print(which(colnames(df_merged_for_cycle_i) == "original_file"))

    # colnames(df_merged_for_cycle_i)[grepl(".x$|.y$",colnames(df_merged_for_cycle_i)) == FALSE] %>%
    #   print(.)

    # subset_test <- df_merged_for_cycle_i %>%
    #   filter(SEQN == 3) %>%
    #   t(.)
    # View(subset_test)
    # which(t(subset_test[1,]) != t(subset_test[2,])) %>% print(.)


    if(cycle_i == -1)
    {
      df_merged_for_cycle_i <- df_merged_for_cycle_i %>%
        mutate(SDDSRVYR = -1) %>%
        mutate(SEQN_new = paste("I-"
                                , SEQN
                                , sep = ""))
    } else {

      df_merged_for_cycle_i <- df_merged_for_cycle_i %>%
        mutate(SDDSRVYR = cycle_i) %>%
        mutate(SEQN_new = paste("C-"
                                , SEQN
                                , sep = ""))

    }

    # View(colnames(df_merged_for_cycle_i) %>% data.frame(.))
    # print("yes")

    df_merged_for_cycle_i <- resolve_duplicates(df_merged_for_cycle_i)
    # print(str(df_merged_for_cycle_i))

    if("SPXBQEFF" %in% colnames(df_merged_for_cycle_i) |
       "SPXBQFV1" %in% colnames(df_merged_for_cycle_i) )
    {

      df_merged_for_cycle_i$SPXBQEFF <- as.character(df_merged_for_cycle_i$SPXBQEFF)
      df_merged_for_cycle_i$SPXBQFV1 <- as.character(df_merged_for_cycle_i$SPXBQFV1)

    } else if("RXDDRGID" %in% colnames(df_merged_for_cycle_i)) {
      df_merged_for_cycle_i$RXDDRGID <- as.character(df_merged_for_cycle_i$RXDDRGID)
    }

    df_merged_for_cycle_i <- remove_all_labels(df_merged_for_cycle_i)
    # print(str(df_merged_for_cycle_i))

    label_cycle <- paste("cycle"
                         , cycle_i
                         , sep = " ")

    codenames_include <- intersect(codenames_include
                                   , colnames(df_merged_for_cycle_i))

    list_all_datasets[[label_cycle]] <- df_merged_for_cycle_i %>%
      select(SEQN
             , SEQN_new
             , SDDSRVYR
             , all_of(codenames_include))

    # df_merged_for_cycle_i <- df_merged_for_cycle_i %>%
    #   select(SEQN
    #          , SEQN_new
    #          , SDDSRVYR
    #          , all_of(codenames_include))

    # if(i == 1)
    # {
    #   merged_nhanes_dataset_final <- df_merged_for_cycle_i
    # } else {
    #   merged_nhanes_dataset_final <- full_join(merged_nhanes_dataset_final
    #                                            , df_merged_for_cycle_i
    #                                            , by = NULL)
    # }
  }

  # Define a function to merge the datasets by column names
  joining_by_colnames <- function(x, y) full_join(x
                                                  , y
                                                  , by = NULL)

  merged_nhanes_dataset_final <- list_all_datasets %>%
    reduce(joining_by_colnames)
  # print(dim(merged_nhanes_dataset_final))

  # merged_nhanes_dataset_final_cleaner <- clean_duplicates_of_seqn_from_cycles(merged_nhanes_dataset_final)

  setwd(current_directory)

  return(merged_nhanes_dataset_final)
}