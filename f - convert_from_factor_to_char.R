convert_from_factor_to_char <- function(dataset_unclean)
{
  
  extract_datatype <- function(x)
  {
    vector_class <- class(x)
    
    length_class <- length(vector_class)
    
    data_type <- vector_class[length_class]
    
    return(data_type)
  }
  
  vector_datatype <- sapply(dataset_unclean
                            , extract_datatype ) %>%
    data.frame(.) 
  
  variable_codenames <- rownames(vector_datatype)

  df_datatype <- data.frame(variable_codename_use = variable_codenames
                            , data_type = unlist(vector_datatype))

  rownames(df_datatype) <- NULL

  # View(df_datatype)

  subset_factors <- df_datatype %>%
    filter(data_type == "factor")
  # View(subset_factors)

  problematic_codenames <- subset_factors %>%
    pull(variable_codename_use) #%>%
    # .[1]

  num_factors <- length(problematic_codenames)
  # print(num_factors)

  if(num_factors == 0)
  {
    dataset_clean <- dataset_unclean

  } else {

    for(i in seq(num_factors))
    {
      variable_codename_i <- problematic_codenames[i]
      # print(variable_codename_i)

      subset_i <- subset_factors %>%
        filter(variable_codename_use == variable_codename_i)

      label_i <- get_label(dataset_unclean[,variable_codename_i])
      # print(label_i)

      # print(str(dataset_unclean[,variable_codename_i]))
      dataset_unclean[,variable_codename_i] <- as.character(dataset_unclean[,variable_codename_i])
      # print(str(dataset_unclean[,variable_codename_i]))

      # print(get_label(dataset_unclean[,variable_codename_i]))

      # Assign the description to that codename
      dataset_unclean[,variable_codename_i] <- set_label(dataset_unclean[,variable_codename_i]
                                                          , label_i)
    }

    dataset_clean <- dataset_unclean
  }
  
  return(dataset_clean)
}