calculate_statistics_for_replicates <- function(df_need_harmonizing
                                                , df_doc_cleaning
                                                , name_dataset = NA)
{
  subset_replicates <- df_doc_cleaning %>%
    filter(statistic_replicates %in% c("rowMeans"
                                       , "rowSums"
                                       , "/")) %>%
    arrange(desc(statistic_replicates))
  # View(subset_replicates)
  
  # print(name_dataset)
  
  if(name_dataset == "Dietary")
  {
    subset_replicates <- subset_replicates %>%
      select(-c(survey_day, file_summary)) %>%
      unique(.)
  }
  # View(subset_replicates)

  codename_statistics <- subset_replicates %>%
    pull(variable_codename_use) %>%
    unique(.) #%>%
    # .[1]

  # codename_statistics <- c("VNLBAVEBPXDI"
  #                          , "VNLDHDLRATIO")

  num_codename_statistics <- length(codename_statistics)

  for(i in seq(num_codename_statistics))
  {
    codename_statistics_i <- codename_statistics[i]


    subset_replicate_i <- subset_replicates %>%
      filter(variable_codename_use == codename_statistics_i)
    # print(subset_replicate_i)

    unique_cycles <- subset_replicate_i %>%
      pull(SDDSRVYR)

    num_cycles <- length(unique_cycles)

    for(j in seq(num_cycles))
    {
      cycle_j <- unique_cycles[j]


      index_cycles_to_change <- which(df_need_harmonizing$SDDSRVYR == cycle_j)

      subset_replicate_i_j <- subset_replicate_i %>%
        filter(SDDSRVYR == cycle_j)
      # print(subset_replicate_i_j)

      statistic <- subset_replicate_i_j %>%
        pull(statistic_replicates)
      # print(length(statistic))

      if(length(statistic) == 2)
      {
        print(codename_statistics_i)
        print(cycle_j)
      }

      string_replicates <- subset_replicate_i_j %>%
        pull(replicate_codenames) %>%
        strsplit(., split = ", ") %>%
        unlist(.) %>%
        paste("'"
              , .
              , "'"
              , sep = "") %>%
        paste(.
              , collapse = ",")

      string_vector_replicates <- string_replicates %>%
        paste("c("
              , .
              , ")"
              , sep = "")

      string_vector_replicates_with_seqn <- string_replicates %>%
        paste("c("
              , "'SEQN_new', "
              , .
              , ")"
              , sep = "")
      # print(string_vector_replicates_with_seqn)



      string_df_measurements <- paste("df_need_harmonizing"
                                      , "[,"
                                      , string_vector_replicates
                                      , "]"
                                      , sep = "")
      # print(string_df_measurements)


      df_measurements <- eval(parse(text = string_df_measurements))

      string_df_measurements_seqn <- paste("df_need_harmonizing"
                                           , "[,"
                                           , string_vector_replicates_with_seqn
                                           , "]"
                                           , sep = "")

      df_measurements_seqn <- eval(parse(text = string_df_measurements_seqn))

      if(statistic %in% c("rowMeans"
                          , "rowSums"))
      {

        row_statistic_string <- paste(statistic
                                      , "(df_measurements, na.rm = TRUE)"
                                      , sep = "")

      } else if(statistic == "/") {

        char_replicates <- subset_replicate_i_j %>%
          pull(replicate_codenames) %>%
          strsplit(., split = ", ") %>%
          unlist(.)
        # print(char_replicates)


        df_measurements_string_replicates <- paste("df_measurements$"
                                                   , char_replicates
                                                   , sep = "")
        # print(df_measurements_string_replicates)


        row_statistic_string <- paste(df_measurements_string_replicates
                                      , collapse = statistic)
      }
      # print(row_statistic_string)


      row_statistic <- eval(parse(text = row_statistic_string))
      # print(unique(row_statistic))

      num_na <- rowSums(is.na(df_measurements))

      num_columns <- ncol(df_measurements)

      df_measurements <- df_measurements_seqn %>%
        mutate(num_na = num_na) %>%
        mutate(row_statistic = row_statistic) %>%
        mutate(row_statistic = ifelse(num_na == num_columns
                                      , NA
                                      , row_statistic))
      # View(df_measurements)

      vector_statistics <- unlist(df_measurements[index_cycles_to_change,"row_statistic"])

      statistic_datatype <- typeof(vector_statistics)
      # print(statistic_datatype)

      if(!(codename_statistics_i %in% colnames(df_need_harmonizing)))
      {
        # print("yes")
        df_need_harmonizing <- df_need_harmonizing %>%
          mutate(empty = NA)

        index_new_variable <- which(colnames(df_need_harmonizing) == "empty")

        colnames(df_need_harmonizing)[index_new_variable] <- codename_statistics_i

      }

      vector_statistics_measurements <- unlist(df_need_harmonizing[index_cycles_to_change,codename_statistics_i])

      measurements_datatype <- typeof(vector_statistics_measurements)
      # print(measurements_datatype)

      if(statistic_datatype != measurements_datatype)
      {
        # print("yes")
        df_need_harmonizing[,codename_statistics_i] <- df_need_harmonizing[,codename_statistics_i]  %>%
          unlist(.) %>%
          as.numeric(.)
        # print(typeof(df_need_harmonizing[index_cycles_to_change,codename_statistics_i]))

        df_measurements[index_cycles_to_change,"row_statistic"] <- vector_statistics %>%
          as.numeric(.)
        # print(typeof(df_measurements[index_cycles_to_change,"row_statistic"]))
      }

      df_need_harmonizing[index_cycles_to_change, codename_statistics_i] <- unlist(df_measurements[index_cycles_to_change
                                                                                                  ,"row_statistic"])

      # columns_to_check <- paste("'SDDSRVYR'"
      #                           , string_replicates
      #                           , paste("'"
      #                                   , codename_statistics_i
      #                                   , "'"
      #                                   , sep = "")
      #                           , sep = ", ") %>%
      #   paste("c("
      #         , .
      #         , ")"
      #         , sep = "")
      # View(df_need_harmonizing[,eval(parse(text = columns_to_check))] %>%
      #        unique(.))
    }

    subset_need_harmonize <- df_need_harmonizing %>%
      select(all_of(codename_statistics_i)
             , SDDSRVYR) %>%
      na.omit(.)

    cycles_chemicals_from_nhanes <- subset_need_harmonize %>%
      pull(SDDSRVYR) %>%
      unique(.)
    # print(cycles_chemicals_from_nhanes)

    subset_chemical_i <- df_doc_cleaning %>%
      filter(variable_codename_use == codename_statistics_i)

    cycles_chemical_i <- subset_chemical_i %>%
      pull(SDDSRVYR) %>%
      unique(.)
    # print(cycles_chemical_i)

    missing_cycles <- outersect(cycles_chemicals_from_nhanes
                                , cycles_chemical_i)

    no_missing_cycles <- is_empty(missing_cycles)

    if(no_missing_cycles == FALSE)
    {
      print(codename_statistics_i)
      print(cycles_chemicals_from_nhanes)
      print(cycles_chemical_i)
      print(missing_cycles)
    }

  }
  df_clean <- df_need_harmonizing
  
  return(df_clean)
}