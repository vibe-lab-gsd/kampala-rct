library(tidyverse)
library(janitor)
library(readxl)
library(openxlsx)
library(googledrive)
library(googlesheets4)


# Directory ------------------------------------------------------------------
git_dir <- "C:/Users/Gray Collins/Documents/GitHub/kampala-rct"
drive_path <- "G:/Shared drives/ugandatransit/uganda_transit_archives"
phase <- "phase3"



# Create route_hourly dataset ------------------------------------------------------------------
# freq_summary_stats_All.xlsx = calculations run in guideform_descriptivestats_hfc.Rmd 
  # G:\Shared drives\ugandatransit\uganda_transit_archives\dataoutput\frequency-intervention-otp\guide-form-summary\phase3

hwmeans_raw <- read.xlsx(file.path(drive_path, r'(\dataoutput\frequency-intervention-otp\guide-form-summary)',
                                   phase, "freq_summary_stats_All.xlsx"))

route_hourly_pre <- hwmeans_raw %>%
  mutate(branch_code = str_extract_all(stage, "^(-*)\\d+(?=:)", simplify = T) %>% as.character()) %>% 
  select(park_name, route_id=route_code, route_name, `Time (start hour)`=hr, `Number of observations`=n,
         `Observed average frequency (oaf)`=mean, iqr_oaf=iqr, p25_oaf=p25, p50_oaf=p50, p75_oaf=p75,
         stage, branch_code)  # %>% 
  # Include only the merger route for any mandatory mergers...  


# Route roster dataset
  # Copy from Google Drive to git 
  stageroster_dir <- "https://docs.google.com/spreadsheets/d/181MMaNac1KcNxV4FQ7d5xUPfKlfILwDw/edit?gid=920893580#gid=920893580"
  
  drive_download(stageroster_dir, path = file.path(git_dir, "data", phase, "stage_intervention_sample_phase3.xlsx"),
                 overwrite = T)

# git method:
stageroster_dir <- file.path(git_dir, "data", phase, "stage_intervention_sample_phase3.xlsx")

stage_roster <- read.xlsx(stageroster_dir, sheet = "stage roster") %>% clean_names() %>% 
  filter(status=="Working with") %>% 
  # Fix double queue for entebbe 
  filter(queue_code!="-15B") %>% select(-c(queue_code, queue))

route_roster <- read.xlsx(stageroster_dir, sheet = "phase 3 route list") %>% clean_names() %>%
  rename(treat_route = final_ish_decision_for_stage_meetings,
         route_fare = fares, route_name = routes) %>% 
  filter(!is.na(route_code)) %>% 
  mutate(route_code = tolower(route_code) %>% str_replace_all("-", "_"), 
         across(c(branch_code, queue_code, stage_code), ~as.character(.x))
         # route_code_2 = ifelse(is.na(route_code_2), route_code, route_code_2), 
         # route_name_2 = ifelse(is.na(route_name_2), route_name, route_name_2) %>% str_to_title()
         ) %>%
  # Filter out routes which are NOT ELIGIBLE for treatment
  filter(treat_route=="include") %>% 
  # Add TIME WINDOW, strata & park/stage names from the stage roster tab 
  select(-c(stage, taxi_park)) %>% 
  tidylog::left_join(stage_roster %>% distinct(taxi_park, branch_code, stage, strata, final_time_window), 
                     by='branch_code') %>% 
  mutate(treatment_window = final_time_window %>% na_if("NA"),
         window_start = str_split_i(treatment_window, "-", 1) %>% str_remove_all("am") %>% str_remove_all("pm"),
         window_end = str_split_i(treatment_window, "-", 2) %>% str_remove_all("am") %>% str_remove_all("pm"),
         tstart = ifelse(as.numeric(window_start)<=6, as.numeric(window_start)+12, as.numeric(window_start)),
         tend = ifelse(as.numeric(window_end)<=6, as.numeric(window_end)+12, as.numeric(window_end))) %>% 
  select(park_name=taxi_park, branch_code, stage_code, stage, strata, treat_route, 
         route_name, route_code, # route_name_2, route_code_2, 
         census_route_id, route_length, route_fare,
         treatment_window, window_start, window_end, tstart, tend)


# Join treatment window to hrly means dataset, and Calculate hourly payments: 
  # 1. target frequency (by route)  = half of avg freq, rounded to nearest -5 number
  # 2. # of payments (by route) = (p75 headway - target headway) * (14 / p75 headway) 
  # 3. planned # of payments (by route) = # of payments  rounded to nearest whole number 
route_hourly <- tidylog::inner_join(route_roster, 
                                    route_hourly_pre %>% select(-c(route_name, stage, branch_code, park_name)), 
                                    by = c("route_code"="route_id")) %>% 
  mutate(
    in_treatment_window = case_when(
      `Time (start hour)`>=tstart & `Time (start hour)`<tend ~ 1, 
      .default = 0),
    # route_fare = as.numeric(route_fare)
    ) %>% 
  select(park_name, branch_code, stage, strata, route_code, route_name, #route_code_2, route_name_2, 
         treat_route, treatment_window, `Time (start hour)`, in_treatment_window, `Number of observations`,
         `Observed average frequency (oaf)`, iqr_oaf, p25_oaf, p50_oaf, p75_oaf, 
         route_fare, tstart, tend) %>% 
  select(park_name, branch_code, stage, route_code, route_name, # route_code_2, route_name_2,
         everything()) %>%
  filter(in_treatment_window==1)


# Merger level info - note = uncomment if assigning merged routes in columns route_code_2, route_name_2 in Excel 
route_hourly_mergers <- route_hourly  #%>% 
  # distinct(park_name, branch_code, stage, strata, route_code_2, route_name_2, 
  #          treat_route, treatment_window, `Time (start hour)`, tstart, tend)


# Calculate target frequency by route 
  # FOR MERGERS: calculate average of mean OAF and p75 OAF for use in target freq & payment calculations   
route_hourly_mergers <- route_hourly %>% 
  # If a single route has multiple fares, take the max route 
  rowwise() %>% 
  mutate(route_fare = max(str_split(route_fare, "-", n=Inf, simplify=T), na.rm=T) %>% 
           as.numeric()) %>% 
  group_by(park_name, branch_code, stage, strata, 
           route_code, route_name, # route_code_2, route_name_2, # use _2 if mergers are present 
           tstart, tend, `Time (start hour)`) %>%
  summarise(
    across(.cols = c(`Observed average frequency (oaf)`, p75_oaf),
           .fns = mean),
    max_route_fare = max(route_fare, na.rm = TRUE)   # max fare among routes at this stage 
  )


# Calculate target frequency, using the average of OAF and p75 for merged routes
mergers_target_freq <- route_hourly_mergers %>% 
  group_by(park_name, branch_code, stage, strata,
           route_code, route_name, 
           # route_code_2, route_name_2,  # use for merged routes 
           tstart, tend) %>%
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
  rename(taxi_park = park_name) %>% 
  distinct(taxi_park, stage, branch_code, strata) %>% 
  arrange(strata, taxi_park, branch_code)

write_csv(stage_strata, 
          file.path(git_dir, "data", phase, "stage_strata.csv"))



# Create route-level dataset for mail merge --------------------------------------------------------
# Calculate # of paid seats 
merged_paid_seats <- merged_hourly %>% 
  group_by(route_code) %>% 
  # group_by(route_code_2) %>% 
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

