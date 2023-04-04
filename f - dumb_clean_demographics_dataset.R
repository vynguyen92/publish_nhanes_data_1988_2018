dumb_clean_demographics_dataset <- function(dataset_unclean
                                            , list_document_cleaning
                                            , name_dataset)
{
  dataset_cleaner <- dataset_unclean %>%
    mutate(RIDAGEYR = ifelse(SDDSRVYR == -1
                             , HSAGEIR
                             , RIDAGEYR)) %>%
    mutate(RIAGENDR = ifelse(SDDSRVYR == -1
                             , HSSEX
                             , RIAGENDR)) %>%
    mutate(INDFMPIR = ifelse(SDDSRVYR == -1
                             , DMPPIR
                             , INDFMPIR)) %>%
    mutate(RIDRETH1 = as.double(RIDRETH1)) %>%
    mutate(RIDRETH1 = case_when(SDDSRVYR != -1 ~ RIDRETH1
                                , SDDSRVYR == -1 & DMARETHN == 1 ~ 3
                                , SDDSRVYR == -1 & DMARETHN == 2 ~ 4
                                , SDDSRVYR == -1 & DMARETHN == 3 ~ 1
                                , SDDSRVYR == -1 & DMARETHN == 4 & DMAETHNR == 3 ~ 5
                                , SDDSRVYR == -1 & DMARETHN == 4 & DMAETHNR == 2 ~ 2)) %>%
    mutate(INDFMPIR = ifelse(INDFMPIR == 888888
                             , NA
                             , INDFMPIR)) %>%
    select(SEQN
           , SEQN_new
           , SDDSRVYR
           , RIDAGEYR
           , RIAGENDR
           , RIDRETH1
           , INDFMPIR) 
  # View(dataset_cleaner)
  
  # View(dataset_cleaner %>%
  #        select("RIDRETH1", "DMARETHN", "DMARACER" ,"DMAETHNR") %>%
  #        unique(.))
  
  attr(dataset_cleaner$SEQN, "label") <- "Respondent sequence number"
  attr(dataset_cleaner$SEQN_new, "label") <- "Respondent sequence number"
  attr(dataset_cleaner$SDDSRVYR, "label") <- "Release cycle number"
  attr(dataset_cleaner$RIDAGEYR, "label") <- "Age in years of the participant at the time of screening"
  attr(dataset_cleaner$RIAGENDR, "label") <- "Gender of the participant."
  attr(dataset_cleaner$RIDRETH1, "label") <- "Recode of reported race and Hispanic origin information"
  attr(dataset_cleaner$INDFMPIR, "label") <- "Poverty income ratio (PIR) - a ratio of family income to poverty threshold"
  
  return(dataset_cleaner)
}