library(tidyverse)
library(janitor)
library(readxl)
library(openxlsx)
library(googledrive)
library(googlesheets4)


# NOTE FOR SUBSEQUENT PHASES: 
# Before running this code, copy the stage roster into the git repo. The stage roster
# should contain the strata and treatment window start & end time for all observations.

# Directory ------------------------------------------------------------------
git_dir <- "C:/Users/Gray Collins/Documents/GitHub/kampala-rct"
drive_path <- "G:/Shared drives/ugandatransit/uganda_transit_archives"
phase <- "phase2"


# # Guide form data ---------------------------------------------------------------
# load(file.path(drive_path, "dataprocessed/frequency-intervention-otp/guide-form", phase, "guideformdatasets_clean.RDS"), verbose = TRUE)
# rm(guideform)



# Create route_hourly dataset ------------------------------------------------------------------
# Created in guideform_descriptivestats_hfc.Rmd 
  # G:\Shared drives\ugandatransit\uganda_transit_archives\dataoutput\frequency-intervention-otp\guide-form-summary\phase2

hwmeans_raw <- read.xlsx(file.path(r'(G:\Shared drives\ugandatransit\uganda_transit_archives\dataoutput\frequency-intervention-otp\guide-form-summary)',
                                   phase, "freq_summary_stats_All.xlsx"))

route_hourly_pre <- hwmeans_raw %>%
  mutate(branch_code = str_extract_all(stage, "^(-*)\\d+(?=:)", simplify = T) %>% as.character()) %>% 
  select(park_name, route_id=route_code, route_name, `Time (start hour)`=hr, `Number of observations`=n,
         `Observed average frequency (oaf)`=mean, iqr_oaf=iqr, p25_oaf=p25, p50_oaf=p50, p75_oaf=p75,
         stage, branch_code) %>% 
  # Include only the merger route for 95 
  filter(route_id == "meng_0001_n" | stage!="95: Mengo-Rubaga-Kosovo-Lusaze")


# Route roster dataset
  # Copy from Google Drive to git 
  stageroster_dir <- "https://docs.google.com/spreadsheets/d/11XpAT7Y_zJrpe4AO8xbD9Z7NtUJsEiLk/edit?gid=74372498#gid=74372498"
  
  drive_download(stageroster_dir, path = file.path(git_dir, "data", phase, "Roster of phase 2 stages (descriptive survey take 1)_v2.xlsx"),
                 overwrite = T)

# git method:
stageroster_dir <- file.path(git_dir, "data", phase, "Roster of phase 2 stages (descriptive survey take 1)_v2.xlsx")
route_roster_raw <- read.xlsx(stageroster_dir, sheet = "route roster_v2") %>% clean_names()

route_roster <- route_roster_raw %>%
  rename(treatment_window=final_treatment_window) %>% 
  mutate(treatment_window = treatment_window %>% na_if("NA"),
         branch_code = as.character(branch_code),
         park_name = as.character(park_name),
         window_start = str_split_i(treatment_window, "-", 1),
         window_end = str_split_i(treatment_window, "-", 2), 
         tstart = ifelse(as.numeric(window_start)<=6, as.numeric(window_start)+12, as.numeric(window_start)),
         tend = ifelse(as.numeric(window_end)<=6, as.numeric(window_end)+12, as.numeric(window_end)),
         route_code_2 = ifelse(is.na(route_code_2), route_code, route_code_2), 
         route_name_2 = ifelse(is.na(route_name_2), route_name, route_name_2) %>% str_to_title()) %>%
  # Filter out routes which are NOT ELIGIBLE for treatment
  filter(treat_route!="No")


# Join treatment window to hrly means dataset, and Calculate hourly payments: 
  # 1. target frequency (by route)  = half of avg freq, rounded to nearest -5 number
  # 2. # of payments (by route) = (p75 headway - target headway) * (14 / p75 headway) 
  # 3. planned # of payments (by route) = # of payments  rounded to nearest whole number 
route_hourly <- tidylog::inner_join(route_roster %>% select(route_code, route_code_2, route_name_2, strata, 
                                                            treatment_window, treat_route, tstart, tend, route_fare), 
                                    route_hourly_pre, by = c("route_code"="route_id")) %>% 
  mutate(
    in_treatment_window = case_when(
      `Time (start hour)`>=tstart & `Time (start hour)`<tend ~ 1, 
      .default = 0),
    route_fare = as.numeric(route_fare)) %>% 
  select(park_name, branch_code, stage, strata, route_code, route_name, route_code_2, route_name_2, treat_route,
         treatment_window, `Time (start hour)`, in_treatment_window, `Number of observations`,
         `Observed average frequency (oaf)`, iqr_oaf, p25_oaf, p50_oaf, p75_oaf, 
         route_fare, tstart, tend) %>% 
  select(park_name, branch_code, stage, route_code, route_name, route_code_2, route_name_2, everything()) %>%
  filter(in_treatment_window==1)


# Merger level info
route_hourly_mergers <- route_hourly %>% 
  distinct(park_name, branch_code, stage, strata, route_code_2, route_name_2, treat_route,
           treatment_window, `Time (start hour)`, tstart, tend)


# Calculate target frequency by route 
  # FOR MERGERS: calculate average of mean OAF and p75 OAF for use in target freq & payment calculations   
route_hourly_mergers <- route_hourly %>% 
  group_by(park_name, branch_code, stage, strata, route_code_2, route_name_2, 
           tstart, tend, `Time (start hour)`) %>%
  summarise(
    across(.cols = c(`Observed average frequency (oaf)`, p75_oaf),
           .fns = mean),
    max_route_fare = max(route_fare, na.rm = TRUE)   # max fare among routes at this stage 
  )


# Calculate target frequency, using the average of OAF and p75 for merged routes
mergers_target_freq <- route_hourly_mergers %>% 
  group_by(park_name, branch_code, stage, strata, route_code_2, route_name_2, tstart, tend) %>%
  summarise(
    target_freq_route = round(mean(`Observed average frequency (oaf)`/2, na.rm=T)/5)*5,  # avg frequency/2 , rounded to nearest 5
  )


# Calculate payments by route 
merged_hourly <- tidylog::full_join(route_hourly_mergers, mergers_target_freq) %>% 
  mutate(
    calculated_payments = (p75_oaf - target_freq_route) * (14/p75_oaf),   # offset, in # of seats, needed to move a departure from p75 to target time
    )


# Save 
write_csv(route_hourly %>%  select(-tstart, -tend), file.path(git_dir, "data", phase, "route_hourly.csv"))
          
write_csv(merged_hourly %>% ungroup() %>% select(-tstart, -tend), 
          file.path(git_dir, "data", phase, "merged_hourly.csv"))




# Create stage_strata dataset --------------------------------------------------------
stage_strata <- route_roster %>% 
  distinct(taxi_park, stage, branch_code, strata) %>% 
  arrange(strata, taxi_park, branch_code)

write_csv(stage_strata, 
          file.path(git_dir, "data", phase, "stage_strata.csv"))



# Create route-level dataset for mail merge --------------------------------------------------------
# Calculate # of paid seats 
merged_paid_seats <- merged_hourly %>% 
  group_by(route_code_2) %>% 
  summarise(
    max_route_fare = max(max_route_fare),
    paid_seats = round(mean(calculated_payments, na.rm=T)),  # rounded average across 4-hr period    
    payment_value = paid_seats * max_route_fare  # fare * estimated # of seats paid per hour
  )

# Join with payment_info 
payment_info <- tidylog::full_join(mergers_target_freq, merged_paid_seats) %>% 
  mutate(stage = str_remove(stage, "^\\d*: "),
         tstart = as.character(tstart) %>% str_c(.,":00"),
         tend = as.character(tend) %>% str_c(.,":00")) 

write_csv(payment_info, 
          file.path(git_dir, "data", phase, "payment_info.csv"))

