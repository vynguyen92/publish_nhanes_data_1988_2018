form_wide_original_codenames_files <- function(x
                                 , list_documentations)
{
  name_dataset_i <- x
  print(name_dataset_i)
  
  df_file_name_subsection_nhanes_iii <- data.frame(file_name = c("ADULT"
                                                                 , "YOUTH"
                                                                 , "LAB"
                                                                 , "EXAM"
                                                                 , "lab2"
                                                                 , "PUPREMED"
                                                                 , "examdr"
                                                                 , "LABSE"
                                                                 , "LAB2SE"
                                                                 , "EXAMSE"
                                                                 , "CMV"
                                                                 , "SPSCMVOD")
                                                   , subsection = c( "1a"
                                                                    , "1a"
                                                                    , "1a"
                                                                    , "1a"
                                                                    , "2a"
                                                                    , "2a"
                                                                    , "2a"
                                                                    , "3a"
                                                                    , "3a"
                                                                    , "3a"
                                                                    , "19a"
                                                                    , "21a"))
  
  if("replicate_codenames" %in% colnames(list_documentations[[name_dataset_i]]))
  {
    subset_files_i <- list_documentations[[name_dataset_i]] %>%
      select(variable_codename_use
             , variable_codename
             , variable_description_use
             , file_name
             , file_category
             , year	
             , SDDSRVYR
             , replicate_codenames)
  } else {
    subset_files_i <- list_documentations[[name_dataset_i]] %>%
      select(variable_codename_use
             , variable_codename
             , variable_description_use
             , file_name
             , file_category
             , year	
             , SDDSRVYR)
  }
  
  subset_files_i <- subset_files_i %>%
    mutate(year = gsub("[A-Za-z]","", year) %>%
             gsub("\\s-\\s", "", .) %>%
             gsub("\\s*$", "", .)) %>%
    unique(.) %>%
    mutate(in_dataset = str_to_title(name_dataset_i)) %>%
    left_join(.
              , df_file_name_subsection_nhanes_iii
              , by = "file_name") %>%
    mutate(weblink = ifelse(SDDSRVYR == -1
                            , paste("https://wwwn.cdc.gov/nchs/data/nhanes3/"
                                    , subsection
                                    , "/"
                                    , file_name %>%
                                      tolower(.)
                                    , "-acc"
                                    , ifelse(file_name == "CMV"
                                             , ".htm"
                                             , ".pdf")
                                    , sep = "")
                            ,  paste("https://wwwn.cdc.gov/Nchs/Nhanes/"
                                     , year
                                     , "/"
                                     , file_name
                                     , ".htm"
                                     , sep = ""))) %>%
    mutate(weblink = ifelse(in_dataset == "Mortality"
                            , "https://www.cdc.gov/nchs/data-linkage/mortality-public.htm"
                            , weblink)) %>%
    mutate(weblink = ifelse(grepl("^fped", file_name) == TRUE
                            , paste("www.ars.usda.gov/arsuserfiles/80400530/pdf/fped/fped_"
                                    , year %>%
                                      gsub("20|_20", "", .) %>%
                                      gsub("-", "", .)
                                    , ".pdf"
                                    , sep = "")
                            , weblink)) %>%
    mutate(weblink = ifelse(grepl("^pyr_tot", file_name) == TRUE & SDDSRVYR %in% c(1,2)
                            , "https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/mypyramid-equivalents-product-downloads/"
                            , ifelse(grepl("^pyr_tot", file_name) == TRUE & SDDSRVYR == 3
                                     , "https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/mped-databases-for-downloading/"
                                     , weblink))) %>%
    mutate(original_codenames = variable_codename)
    # mutate(original_name_weblink = paste(variable_codename
    #                                      , " ("
    #                                      , 
    #                                      , ")"))
  # View(subset_files_i)
  
  
  # print(colnames(subset_files_i))
  
  if("replicate_codenames" %in% colnames(subset_files_i))
  {
    subset_files_i <- create_weblinks_for_derived_variables(subset_files_i
                                                            , df_file_name_subsection_nhanes_iii)
  }
  
  if("ADULT" %in% subset_files_i$file_name & "YOUTH" %in% subset_files_i$file_name)
  {
    subset_files_i <- merge_weblinks_for_adult_and_youth_variables(subset_files_i)
  }
  

  
  # print(subset_files_i %>% 
  #        filter(SDDSRVYR == -1) %>%
  #        select(file_name
  #               , SDDSRVYR
  #               , subsection) 
  #       %>% unique(.))
  
  # if(name_dataset_i == "Chemicals")
  # {
  #   comments_i <- list_documentations[[name_dataset_i]] %>%
  #     select(comment_codename_use
  #            , variable_description_use
  #            , units
  #            , file_name
  #            , file_category
  #            , year	
  #            , SDDSRVYR) %>%
  #     drop_na(comment_codename_use) %>%
  #     unique(.) %>%
  #     rename(variable_codename_use = comment_codename_use) %>%
  #     mutate(in_dataset = "Comments")
  #   # View(comments_i)
  #   
  #   subset_files_i <- subset_files_i %>%
  #     full_join(.
  #               , comments_i
  #               , by = colnames(.))
  #   
  # }
  
  return(subset_files_i)
}