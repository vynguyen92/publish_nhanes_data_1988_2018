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
  
  subset_files_i <- list_documentations[[name_dataset_i]] %>%
    select(variable_codename_use
           , variable_codename
           , variable_description_use
           , file_name
           , file_category
           , year	
           , SDDSRVYR) %>%
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
                            , weblink))
    # mutate(original_name_weblink = paste(variable_codename
    #                                      , " ("
    #                                      , 
    #                                      , ")"))
  # View(subset_files_i)
  
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