##--------------------------------------------------------------------------------------------------------
## SCRIPT : Merging General and Specific Coding sheets
##
## Authors : Matthieu Authier, Sean Heighton
## Last update : 2025-07-03
##
## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"
## Copyright (C) 2020 The R Foundation for Statistical Computing
## Platform: x86_64-w64-mingw32/x64 (64-bit)
##--------------------------------------------------------------------------------------------------------

what_u_need <- c("lubridate", "tidyverse", "readxl")
cran_packages <- what_u_need[!(what_u_need %in% installed.packages())]
if(length(cran_packages) != 0) {
  lapply(cran_packages, install.packages, dependencies = TRUE)
}
lapply(what_u_need, library, character.only = TRUE)

rm(list = ls())

#This script is used to analyse merge the General and Specific Coding sheets

# ----------------- #
#
# Prepare data & background for script
#
# ----------------- #

#Setup background
data_path <- "./generalcoding/data/"

File = "General_Coding_06-05-2025_&_metadata.csv" #download the General coding file as csv from drive, remove first two rows
Filename = "General_coding"

#Upload General Coding csv (already cleaned for newlines)
coding <- read.csv(paste0(data_path, File), stringsAsFactors = FALSE)

coding %>%
  head(n = 1)

# Load Data; eg CH5
CHFILE = "20260623_Copie de Final_Coding_Tool_CHAP_5_CEM_only.xlsx"
coding_CH <- readxl::read_excel(paste0("./data/raw/", CHFILE), trim_ws = TRUE, sheet = 3) %>%
  # clean empty column
  select(where(function(x) any(!is.na(x)))) %>%
  rename(Analysis_order = AI_Analysis_order,
         Pressure..level.2. = Pressure
         )

coding_CH %>%
  pull(Pressure_comment) %>%
  table()

output_path <- "./data/output"

# check
any(names(coding_CH) == "Analysis_order")
any(names(coding_CH) == "Target")
any(names(coding_CH) == "Pressure..level.2.")

# source some useful functions
source("./common_functions/20250521_merging_fct.r")

#### make some local corrections
coding_CH[which(coding_CH$AI_Key == "PKN9TKAV"), "Pressure..level.2."] <- coding[which(coding$Key == "PKN9TKAV"), "Pressure..level.2."]
coding_CH[which(coding_CH$AI_Key == "33ETT36M"), "Pressure..level.2."] <- coding[which(coding$Key == "33ETT36M"), "Pressure..level.2."]

####
merged_data_FINAL <- merge_coding_sheets(specific = coding_CH,
                                         general = coding,
                                         analysis_order_only = FALSE,
                                         export_output = TRUE,
                                         output_filename = "Final_Coding_Tool_CHAP_5",
                                         output_path = output_path
                                         ) %>%
  mutate(match_status = case_when(str_detect(Pressure_comment, "EXCLUDED") ~ "exclude",
                                  str_detect(Pressure_comment, "Excluded") ~ "exclude",
                                  str_detect(Pressure_comment, "Exclued") ~ "exclude",
                                  .default = match_status
                                  )
         )

### check 
merged_data_FINAL %>%
  filter(!is.na(P_hypothesis)) %>%
  pull(match_status) %>%
  table()

# ----------------- #
#
#### Unnesting and splitting OWF metadata - long version ####
#
# ----------------- #
# If we want to analyse questions related to OWF metadata (turbine number, type of EUNIS habitat etc), 
# we need to unnest this data when multiple farms were in one study
merged_data_FINAL_OWFunnested <- merged_data_FINAL %>%
  filter(!is.na(P_hypothesis)) %>%
  unnest_owf_data(merged_df = .,
                  export_output = TRUE,
                  output_filename = "Final_Coding_Tool_CHAP_5",
                  output_path = output_path
                  )

merged_data_FINAL_OWFunnested %>%
  View()
