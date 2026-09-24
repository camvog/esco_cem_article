##--------------------------------------------------------------------------------------------------------
## SCRIPT : Chapter 5
##
## Authors : Camille Vogel, Matthieu Authier,  Sean Heighton, 
## Last update : 2026-02-17
##
## R version 4.5.1 
## Copyright (C) 2024 The R Foundation for Statistical Computing
## Platform: x86_64-w64-mingw32/x64
##--------------------------------------------------------------------------------------------------------

what_u_need <- c("lubridate", "tidyverse", "readxl", "ggbeeswarm", "skimr")
cran_packages <- what_u_need[!(what_u_need %in% installed.packages())]
if(length(cran_packages) != 0) {
  lapply(cran_packages, install.packages, dependencies = TRUE)
}
lapply(what_u_need, library, character.only = TRUE)

rm(list = ls())
source("./common_functions/esco_functions.r")

mergedata <- read.csv(file = "./CH5/output/Final_Coding_Tool_CHAP_5_GeneralCoding_merged.csv") %>%
  filter(match_status == "merged")

# correction complementaire (filtre de 2 refs a exclure)
mergedata_2 <- mergedata %>% filter(is.na(Pressure_comment) | !str_detect(Pressure_comment, regex("EXCLUDED", ignore_case = TRUE)))

##--------------------------------------------------------------------------------------------------------
## Sens de variation par cat. d'indicateurs
##--------------------------------------------------------------------------------------------------------

df_cat_emf <- mergedata_2 %>% 
  filter(!is.na(EXPMG_Type)
  ) %>%
  select(  AI_Key, AI_Short_Author, AI_Publication_Year
         , Target, AIP_Taxonomicposition, PLSS_lifestage1
         , Empirical..ex.situ..laboratory.studies.
         , starts_with("OICM_")
         , starts_with("OICIE_")
         , starts_with("CVESTE_")
  ) %>%
  rename(Species = AIP_Taxonomicposition
         , Life_stage = PLSS_lifestage1
         , exsitu = Empirical..ex.situ..laboratory.studies.
         )%>%
  mutate(exsitu = case_when(exsitu == "No" ~ "In vivo",
                                     exsitu == "Yes" ~ "In vitro",
                                     .default = exsitu
         )
  )

df_fish <-  df_cat_emf %>% filter(Target == "Fish")
df_benthos <- df_cat_emf %>% filter(Target !="Fish")


  
