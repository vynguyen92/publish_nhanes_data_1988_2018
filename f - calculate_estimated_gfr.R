#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##############################  FUNCTION TO CALCULATE GLOMERULAR FILTRATION RATE  #############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function will calculate the glomerular filtration rate (GFR) for each participants and add a 
#          column of this estimation to the merged dataset
#          
# Inputs: merged_dataset - data frame containing the demographic, mortality, and biomarker info for each  
#                          participant
#
# Outputs: merged_dataset - merged dataset with a column vector for GFR

calculate_estimated_gfr <- function(dataset_response
                                    , dataset_demographics)
{
  merged_dataset <- left_join(dataset_response
                              , dataset_demographics
                              , by = c("SEQN_new"
                                       , "SEQN"
                                       , "SDDSRVYR"))
  
  # Define vectors of indices for demographic features 
  index_adults <- which(merged_dataset$RIDAGEYR >= 18)
  index_children <- which(merged_dataset$RIDAGEYR < 18)
  index_male <- which(merged_dataset$RIAGENDR == 1)
  index_female <- which(merged_dataset$RIAGENDR == 2)
  index_black <- which(merged_dataset$RIDRETH1 == 4)
  index_not_black <- which(merged_dataset$RIDRETH1 != 4)
  
  # Define list to contain indices for sex and race
  sex_list <- list(male = index_male, female = index_female)
  race_list <- list(black = index_black, not_black = index_not_black)
  
  # Initiate an empty column for GFR
  merged_dataset[,"VNEGFRADJ"] <- rep(NA, nrow(merged_dataset)) %>%
    as.numeric(.)
  merged_dataset[,"VNEGFR"] <- rep(NA, nrow(merged_dataset)) %>%
    as.numeric(.)
  
  # Perform for each age group (children and adults)
  for(age_group in seq(2))
  {
    
    # For adults, use https://www.niddk.nih.gov/health-information/communication-programs/nkdep/laboratory-evaluation/glomerular-filtration-rate-calculators/ckd-epi-adults-conventional-units
    if(age_group == 1)
    {
      # Perform for each sex
      for(sex in seq(2))
      {
        # print(names(sex_list[sex]))
        
        # Define the values of the parameters used in calculating GFR
        if(names(sex_list[sex]) == "female"){
          
          k <- 0.7
          a <- -0.329
          sex_constant <- 1.018
          
        } else {
          
          k <- 0.9
          a <- -0.411
          sex_constant <- 1
        }
        
        # Perform for each race
        for(race in seq(2))
        {
          # print(names(race_list[race]))
          # Determine the indices of participants who are a given race and sex 
          index_race_sex_combo <- Reduce(intersect, list(index_adults
                                                         , sex_list[[sex]]
                                                         , race_list[[race]]))
          # print(index_race_sex_combo)
          
          # Define the values of the race-specific parameters used in calculating GFR
          if(names(race_list[race]) == "black")
          {
            race_constant <- 1.159
            
          } else {
            race_constant <- 1
          }
          
          # merged_dataset[index_race_sex_combo,"VNEGFR"] <- 141*(0.993^merged_dataset$RIDAGEYR[index_race_sex_combo])
          
          vector_ones <- rep(1, length(index_race_sex_combo))
          
          # Calculate the GFR
          gfr_without_race <- 141*
            (pmin(merged_dataset$LBXSCR[index_race_sex_combo]/k,vector_ones)^a)*
            (pmax(merged_dataset$LBXSCR[index_race_sex_combo]/k,vector_ones)^-1.209)*
            (0.993^merged_dataset$RIDAGEYR[index_race_sex_combo])*
            sex_constant
          gfr_with_race <- gfr_without_race*
            race_constant
          
          merged_dataset[index_race_sex_combo,"VNEGFRADJ"] <- gfr_with_race
          merged_dataset[index_race_sex_combo,"VNEGFR"] <- gfr_without_race
          # print(merged_dataset$LBXSCR[index_race_sex_combo]/k)
          
        }
      }
      
    # For children, use https://www.niddk.nih.gov/health-information/communication-programs/nkdep/laboratory-evaluation/glomerular-filtration-rate-calculators/children-conventional-units
    } else {
      height_children <- merged_dataset$BMXHT[index_children]
      serum_cr_children <- merged_dataset$LBXSCR[index_children]
      merged_dataset[index_children,"VNEGFRADJ"] <- (0.41*height_children)/serum_cr_children
      merged_dataset[index_children,"VNEGFR"] <- (0.41*height_children)/serum_cr_children
    }
    
  }
  
  df_adults <- merged_dataset %>%
    select("VNEGFR"
           ,"VNEGFRADJ"
           ,"RIDAGEYR"
           , "RIAGENDR"
           , "RIDRETH1"
           , "LBXSCR"
           , "RIAGENDR") %>%
    filter(is.na(VNEGFR) == TRUE) %>%
    filter(RIDAGEYR >= 18) %>%
    unique(.)
  # View(df_adults)
  
  df_children <- merged_dataset %>%
    select("VNEGFR"
           ,"VNEGFRADJ"
           , "RIDAGEYR"
           , "LBXSCR"
           , "BMXHT") %>%
    filter(is.na(VNEGFR) == TRUE) %>%
    filter(RIDAGEYR < 18) %>%
    unique(.)
  # View(df_children)
  
  # merged_dataset <- merged_dataset %>%
  #   select(-c(RIDAGEYR, RIAGENDR, RIDRETH1))
  
  return(merged_dataset)
}