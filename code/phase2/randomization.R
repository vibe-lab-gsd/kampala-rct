library(tidyverse)
library(readxl)
library(googledrive)
library(googlesheets4)

git_dir <- "C:/Users/Gray Collins/Documents/GitHub/kampala-rct"
phase <- "phase2"

strata <- read_csv(file.path(git_dir, "data", phase, "stage_strata.csv"))

set.seed(4982392)

samp <-   strata %>%
  arrange(branch_code) %>%
  group_by(strata) %>%
  group_modify(~ {
    n <- nrow(.x)
    # base sample size is floor(n/2)
    base_n <- n %/% 2
    # if odd, flip a coin to add 1
    extra <- if (n %% 2 == 1 && runif(1) < 0.5) 1 else 0
    slice_sample(.x, n = base_n + extra)
  }) %>%
  ungroup() %>%
  mutate(treatment = 1L)

treatment_groups <- left_join(strata, samp %>% select(branch_code, treatment), 
          by="branch_code") %>% 
  mutate(group = case_when(treatment==1 ~ "Treatment", 
                           is.na(treatment) ~ "Control")) %>% 
  select(-treatment)
 
print(treatment_groups, n=60)


write_csv(treatment_groups,
          file.path(git_dir, "data", phase, "treatment_groups.csv"))

