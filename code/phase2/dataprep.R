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
  # Copy from Google Drive to git 
  stageroster_dir <- "https://docs.google.com/spreadsheets/d/1_cG4STjIpBwmfXpxJtGoye7L1waCCFrMW2PIjK8ljEE/edit?gid=1631865340#gid=1631865340"
  drive_download(stageroster_dir, path = file.path(git_dir, "data", phase, "Roster of phase 2 stages (descriptive survey take 1).xlsx"),
                 overwrite = T)


# google drive method: (live doc)
route_roster_raw <- drive_get(as_id(stageroster_dir)) %>% range_read(sheet = 'route roster') %>% clean_names()

# # git method:
# stageroster_dir <- file.path(git_dir, "data", phase, "Roster of phase 2 stages (descriptive survey take 1).xlsx")
# route_roster_raw <- read.xlsx(stageroster_dir, sheet = "route roster") %>% clean_names() 

route_roster <- route_roster_raw %>%
  rename(treatment_window=final_treatment_window) %>% 
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
route_hourly <- tidylog::inner_join(route_roster %>% select(route_code, strata, treatment_window, tstart, tend, stage_fares, route_fare), 
                                    route_hourly_pre, by = c("route_code"="route_id")) %>% 
  mutate(
    in_treatment_window = case_when(
      `Time (start hour)`>=tstart & `Time (start hour)`<tend ~ 1, 
      .default = 0),
    route_fare = as.numeric(route_fare)) %>% 
  select(park_name, branch_code, stage, strata, route_code, route_name, 
         treatment_window, `Time (start hour)`, in_treatment_window, `Number of observations`,
         `Observed average frequency (oaf)`, iqr_oaf, p25_oaf, p50_oaf, p75_oaf, 
         stage_fares, route_fare, tstart, tend) %>% 
  # Drop rows where NO treatment will be assigned 
  filter(!is.na(treatment_window)) %>%
  # Combine routes for relevant stages 
  mutate(
    route_code_2 = case_when(
      route_code %in% c("luga_luga_g", "luga_masa_g", "luga_nase_g") ~ "luga_0001_g", 
      route_code %in% c("luga_buye_g", "luga_kawu_g", "luga_bujo_g") ~ "luga_0002_g",
      route_code %in% c("ndej_kana", "ndej_lubu") ~ "ndej_0001", 
      route_code %in% c("ndej_kibu", "ndej_zant") ~ "ndej_0002",
      .default = route_code
    ), 
    route_name_2 = case_when(
      route_code_2 == "luga_0001_g" ~ "Lugala / Masanafu / Nasere (MERGED)",
      route_code_2 == "luga_0002_g" ~ "Buyera-Temangaro / Kawoko / Bujjuko (MERGED)",
      route_code_2 == "ndej_0001" ~ "Kanaaba / Lubugumu Kakola (MERGED)",
      route_code_2 == "ndej_0002" ~ "Kibutika / Zanta (MERGED)",
      .default = route_name
    ) %>% str_to_title()
  ) %>% 
  # Calculate hrly payments (by route)
  group_by(route_code_2) %>%
  mutate(
    target_freq_perhr_raw = `Observed average frequency (oaf)`/2,
    target_freq_perhr = floor(target_freq_perhr_raw/5)*5,   # round to nearest 5 
    calculated_payments_perhr = (p75_oaf - target_freq_perhr) * (14/p75_oaf),   # payments per hr 
    paid_seats_perhr = round(calculated_payments_perhr)   # rounded payments per hr 
  ) %>% 
  select(park_name, branch_code, stage, route_code, route_name, route_code_2, route_name_2, everything())

# # Calculate target frequency by stage (average across all target freqs by stage)
# stagelvl <- route_hourly %>% 
#   filter(in_treatment_window==1) %>% 
#   group_by(stage) %>% 
#   summarise(target_freq_stage = floor(mean(target_freq_perhr, na.rm=T)/5)*5)
# 
# # Calculate target frequency by route (average across all target freqs for single route)
# routelvl <- route_hourly %>% 
#   filter(in_treatment_window==1) %>% 
#   group_by(route_code) %>% 
#   summarise(target_freq_route = floor(mean(target_freq_perhr, na.rm=T)/5)*5)
# 
# # Bind rows & calculate payment targets 
# route_hourly <- route_hourly %>% 
#   left_join(stagelvl) %>% 
#   left_join(routelvl) %>%
#   mutate(
#     # Using HOUR- AND ROUTE-SPECIFIC target headway: 
#     calculated_payments_perhr = (p75_oaf - target_freq_perhr) * (14/p75_oaf),
#     paid_seats_perhr = round(calculated_payments_perhr),
#     
#     # Using ROUTE-wide target headway (same for every hr in a route): 
#     calculated_payments_route = (p75_oaf - target_freq_route) * (14/p75_oaf),
#     paid_seats_route = round(calculated_payments_route),
#     payment_per_departure_route = paid_seats_route * route_fare, 
#     
#     # Using STAGE-wide target headway (same for every hr in a stage): 
#     calculated_payments_stage = (p75_oaf - target_freq_stage) * (14/p75_oaf),
#     paid_seats_stage = round(calculated_payments_stage),
#     payment_per_departure_stage = paid_seats_stage * route_fare 
#   )


# Save 
write_csv(route_hourly %>% select(-tstart, -tend), 
          file.path(git_dir, "data", phase, "route_hourly.csv"))

 

# Create stage_strata dataset --------------------------------------------------------
stage_strata <- route_roster %>% 
  distinct(taxi_park, stage, branch_code, strata) %>% 
  arrange(strata, taxi_park, branch_code)

write_csv(stage_strata, 
          file.path(git_dir, "data", phase, "stage_strata.csv"))



# Create route-level dataset for mail merge --------------------------------------------------------
payment_info <- route_hourly %>% 
  # filter to only rows within the treatment window 
  filter(in_treatment_window==1) %>% 
  # average frequency per hour, by stage 
  group_by(stage) %>% 
  mutate(target_freq_stage = floor(mean(target_freq_perhr, na.rm=T)/5)*5) %>% 
  # average frequency per hour, by route 
  group_by(park_name, branch_code, stage, strata, route_code_2, route_name_2, route_fare, tstart, tend, target_freq_stage) %>% 
  summarise(paid_seats = mean(paid_seats_perhr, na.rm=T),
            target_freq_route =  floor(mean(target_freq_perhr, na.rm=T)/5)*5) %>% 
  mutate(payment_value = route_fare * round(paid_seats), 
         stage = str_remove(stage, "^\\d*: "),
         tstart = as.character(tstart) %>% str_c(.,":00"),
         tend = as.character(tend) %>% str_c(.,":00")) %>% 
  # drop routes 
  filter(route_code_2 != "ndej_nyan")

write_csv(payment_info, 
          file.path(git_dir, "data", phase, "payment_info.csv"))

