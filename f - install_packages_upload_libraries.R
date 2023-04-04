#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#############################  FUNCTION TO INSTALL AND UPLOAD LIBRARIES  ######################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function serves as a repository of libraries needed for analysis 
#
# Inputs: none 
#
# Outputs: none - necessary libraries are installed and uploaded into RStudio to prepare for analysis

install_packages_upload_libraries <- function()
{
  # Install and upload nhanesA package to obtain access to all NHANES datasets 
  # https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
  install.packages("nhanesA")
  library("nhanesA")
  
  # Install and upload readtext package for reading text to upload mortality data 
  install.packages("readtext")
  library("readtext")
  
  # Install and upload readxl package to help upload datasets from excel
  install.packages("readxl")
  library("readxl")
  
  # Install and upload survival package to have access to survival models
  install.packages("survival")
  library("survival")
  
  # Install and upload survminer package to have access to functions used for making plots from survival data
  install.packages("survminer")
  library("survminer")

  # Install and upload sjlabelled package to dealed with labelled data
  install.packages("sjlabelled")
  library("sjlabelled")
  
  # Install and upload gtable package to help make figures made of multiple plots
  install.packages("gtable")
  library("gtable")
  
  # Upload gtable package to help make figures made of multiple plots and label axes of these figures
  library("gridExtra")
  
  # Upload gtable package to help label plots within the same figure
  library("grid")
  
  install.packages("gplots")
  library(gplots)
  
  # library("Hmisc")
  
  # Upload ggrepel package to help avoid overlapping text labels
  library("ggrepel")
  
  # library("MASS")
  
  # Upload ggrepel package to help with scales of axes in plots
  library("scales")
  
  # Install and upload janitor package to help with examining and cleaning data
  install.packages("janitor")
  library("janitor")
  
  # Install and upload survey package to access function to run survey-weighted regression models
  install.packages("survey")
  library("survey")
  
  # Upload RColorBrewer package to have access to different color schemes to make figures
  library("RColorBrewer")
  
  # Install and upload cowplot package to extract legends to make a composite legend
  install.packages("cowplot")
  library("cowplot")
  
  # Install and upload survMisc package to help with making predicted mortality risk
  install.packages("survMisc")
  library("survMisc")
  
 
  # install.packages("pec")
  # library("pec")
  
  # install.packages("coxrt")
  # library("coxrt")
  
  # Install and upload caret package to help cross-validation
  install.packages("caret")
  library("caret")
  
  # Install and upload glmnet package to help cross-validation for cox models
  install.packages("glmnet")
  library("glmnet")
  
  # Install and upload boot package to help to implement bootstrapping
  install.packages("boot")
  library("boot")
  
  # Install and upload boot package to help to implement bootstrapping
  install.packages("bootstrap")
  library("bootstrap")
  
  # Upload splines package to implement splines models
  library("splines")
  
 
  # library("rms")
  
  # install.packages("Greg")
  # library("Greg")
  
  # Install and upload features package to help with extracting critical points
  install.packages("features")
  library("features")
  
  # Needed to run on Microsoft Open R 3.5.3 for threading on high computing computer
  options(repos = c(CRAN = "https://cran.rstudio.com"))
  
  # Install and upload pillar package to help format columns of data using colors
  # Needed to run analysis on high computing computer 
  install.packages("pillar")
  library("pillar")
  
  # Install and upload vctrs package to help format vectors
  # Needed to run analysis on high computing computer 
  install.packages("vctrs", dependencies = TRUE)
  library("vctrs")
  
  # Install and upload tidyverse package for access to functions to faciliate ease of getting statistics
  install.packages("tidyverse", dependencies = TRUE)
  library("tidyverse")
  
  # Install and upload broom package to help format results of regression models into a tidy dataset
  install.packages("broom", dependencies = TRUE)
  library("broom")
  
  # Upload library to help with parallel computing 
  library("parallel")
  
  # Upload library to help with parallel computing specifically for a for loop
  library("foreach")
  
  # Install and upload library to help with parallel computing 
  install.packages("doParallel")
  library("doParallel")
  
  install.packages("pvclust")
  library("pvclust")
}