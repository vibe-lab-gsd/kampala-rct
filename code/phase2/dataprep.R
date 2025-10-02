library(tidyverse)
library(readxl)
library(googledrive)
library(googlesheets4)


# NOTE FOR SUBSEQUENT PHASES: 
# Before running this code, copy the stage roster into the git repo. The stage roster
# should contain the strata and treatment window start & end time for all observations.

# Directory ------------------------------------------------------------------
git_dir <- "C:/Users/Gray Collins/Documents/GitHub/kampala-rct"
phase <- "phase2"

# Create route_hourly dataset ------------------------------------------------------------------
# Created in guideform_descriptivestats_hfc.Rmd 
  # G:\Shared drives\ugandatransit\uganda_transit_archives\dataoutput\frequency-intervention-otp\guide-form-summary\phase2

hwmeans_raw <- read.xlsx(file.path(r'(G:\Shared drives\ugandatransit\uganda_transit_archives\dataoutput\frequency-intervention-otp\guide-form-summary)',
                                   phase, "freq_summary_stats_All.xlsx"))

route_hourly_pre <- hwmeans_raw %>% 
  mutate(branch_code = str_extract_all(stage, "^(-*)\\d+(?=:)", simplify = T) %>% as.character()) %>% 
  select(park_name, route_id=route_code, route_name, `Time (start hour)`=hr, `Number of observations`=n,
         `Observed average frequency (oaf)`=mean, iqr_oaf=iqr, p25_oaf=p25, p50_oaf=p50, p75_oaf=p75,
         stage, branch_code)



# Route roster dataset
# stageroster_dir <- "https://docs.google.com/spreadsheets/d/1_cG4STjIpBwmfXpxJtGoye7L1waCCFrMW2PIjK8ljEE/edit?gid=1631865340#gid=1631865340"
stageroster_dir <- file.path(git_dir, "data", phase, "Roster of phase 2 stages (descriptive survey take 1).xlsx")

route_roster_raw <- read.xlsx(stageroster_dir, sheet = "route roster")   # drive_get(as_id(stageroster_dir)) %>% range_read(sheet = 'route roster') 

route_roster <- route_roster_raw %>%
  rename(strata=Strata, treatment_window=`FINAL.Treatment.window`, stage_fares = `Stage.fares`) %>% 
  mutate(treatment_window = treatment_window %>% na_if("NA"),
         branch_code = as.character(branch_code),
         park_name = as.character(park_name),
         window_start = str_split_i(treatment_window, "-", 1),
         window_end = str_split_i(treatment_window, "-", 2), 
         tstart = ifelse(as.numeric(window_start)<=6, as.numeric(window_start)+12, as.numeric(window_start)),
         tend = ifelse(as.numeric(window_end)<=6, as.numeric(window_end)+12, as.numeric(window_end)))



# Join treatment window to hrly means dataset, and Calculate hourly payments: 
  # 1. target frequency (by route)  = half of avg freq, rounded to nearest -5 number
  # 2. # of payments (by route) = (p75 headway - target headway) * (14 / p75 headway) 
  # 3. planned # of payments (by route) = # of payments  rounded to nearest whole number 
route_hourly <- tidylog::inner_join(route_roster %>% select(route_code, strata, treatment_window, tstart, tend, stage_fares), 
                                    route_hourly_pre, by = c("route_code"="route_id")) %>% 
  mutate(
    in_treatment_window = case_when(
      `Time (start hour)`>=tstart & `Time (start hour)`<tend ~ 1, 
      .default = 0)) %>% 
  select(park_name, branch_code, stage, strata, route_code, route_name, 
         treatment_window, `Time (start hour)`, in_treatment_window,
         `Observed average frequency (oaf)`, iqr_oaf, p25_oaf, p50_oaf, p75_oaf) %>% 
  filter(in_treatment_window==1) %>% 
  # Calculate hrly payments (by route)
  group_by(route_code) %>% 
  mutate(
    target_freq = `Observed average frequency (oaf)`/2,
    calculated_payments = (p75_oaf - target_freq) * (14/p75_oaf),
    paid_seats = round(calculated_payments)
  ) 


# Save 
write_csv(route_hourly, 
          file.path(git_dir, "data", phase, "route_hourly.csv"))

 

# Create stage_strata dataset --------------------------------------------------------
stage_strata <- route_roster %>% 
  distinct(taxi_park, stage, branch_code, strata) %>% 
  arrange(strata, taxi_park, branch_code)

write_csv(stage_strata, 
          file.path(git_dir, "data", phase, "stage_strata.csv"))




