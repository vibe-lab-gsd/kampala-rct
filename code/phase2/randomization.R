library(tidyverse)
library(readxl)
library(googledrive)
library(googlesheets4)

git_dir <- "C:/Users/Gray Collins/Documents/GitHub/kampala-rct"
phase <- "phase2"

strata <- read_csv(file.path(git_dir, "data", phase, "stage_strata.csv"))

set.seed(192333)

samp <- strata %>% 
  arrange(branch_code) %>% 
  group_by(strata) %>% 
  slice_sample(prop = 0.5) %>% 
  mutate(treatment = 1)

treatment_groups <- left_join(strata, samp %>% select(branch_code, treatment), 
          by="branch_code") %>% 
  mutate(group = case_when(treatment==1 ~ "Treatment", 
                           is.na(treatment) ~ "Control")) %>% 
  select(-treatment)

print(treatment_groups, n=30)


write_csv(treatment_groups,
          file.path(git_dir, "data", phase, "treatment_groups.csv"))

