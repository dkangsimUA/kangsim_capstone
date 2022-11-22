###
# This script downloads the sample Mexico responses from the Qualtrics API
# Note you must register Qualtrics first
###

# load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(qualtRics, glue, tidyverse)

fetch_survey("SV_7ZCxh3OmFBpFWu2", #Survey ID SV_7ZCxh3OmFBpFWu2
             breakout_sets = FALSE,
             add_var_labels = FALSE) %>% 
  filter(Finished == TRUE) %>%  # Filters out in-progress surveys
  write_csv( # save survey results 
    file = here(
      "data_output",
      glue("MEX_registration_output_{Sys.Date()}.csv")),
    na = "")

# print a message in the console

message(glue("Mexico Survey successfully downloaded to\nsurvey_workflow/\n1_registration_survey_output/\n1_survey_results/\nMEX_registration_output_{Sys.Date()}.csv"))