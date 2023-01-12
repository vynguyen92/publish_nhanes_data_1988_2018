#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Account for NHANES sampling design  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(survey)
library(tidyverse)
library(broom)
load("./w - nhanes_1988_2018.RData")

nhanes <- full_join(demographics_clean, 
                    by = c("SEQN",
                           "SEQN_new", 
                           "SDDSRVYR")) %>%
  full_join(., 
            weights_clean, 
            by = c("SEQN",
                   "SEQN_new", 
                   "SDDSRVYR")) %>%
  select("SEQN",
         "SEQN_new", 
         "SDDSRVYR",
         "SDMVPSU",
         "SDMVSTRA",
         "WTMEC2YR",
         "MORTSTAT",
         "PERMTH_INT",
         "RIDAGEYR",
         "RIAGENDR",
         "RIDRETH1") %>%
  na.omit(.)

nhanes <- nhanes %>%
  mutate(RIAGENDR = factor(RIAGENDR),
         RIDRETH1 = relevel(factor(RIDRETH1)
                            , ref = 3))

dsn <- svydesign(ids = ~SDMVPSU, 
                 strata = ~SDMVSTRA, 
                 weights = ~WTMEC2YR, 
                 nest = TRUE, 
                 data = nhanes)

model_svyglm <- svyglm(RIDAGEYR ~ RIAGENDR + RIDRETH1, 
                       design = dsn)
tidy(model_svyglm)
glance(model_svyglm)

model_svycoxph <- svycoxph(Surv(time = PERMTH_INT,
                                event = MORTSTAT) ~ RIDAGEYR + RIAGENDR + RIDRETH1,
                           design = dsn)
tidy(model_svycoxph)
glance(model_svycoxph)
