library(tidyverse)
library(readxl)
library(googledrive)
library(googlesheets4)


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
stageroster_dir <- 

route_roster <- drive_get(as_id(stageroster_dir)) %>% range_read(sheet = 'route roster') %>%
  rename(strata=Strata, treatment_window=`Treatment window`) %>% 
  mutate(treatment_window = treatment_window %>% na_if("NA"),
         branch_code = as.character(branch_code),
         park_name = as.character(park_name),
         window_start = str_split_i(treatment_window, "-", 1),
         window_end = str_split_i(treatment_window, "-", 2), 
         tstart = ifelse(as.numeric(window_start)<=6, as.numeric(window_start)+12, as.numeric(window_start)),
         tend = ifelse(as.numeric(window_end)<=6, as.numeric(window_end)+12, as.numeric(window_end)))



# Join treatment window to hrly means dataset 
route_hourly <- tidylog::inner_join(route_roster %>% select(route_code, strata, treatment_window, tstart, tend), 
                                    route_hourly_pre, by = c("route_code"="route_id")) %>% 
  mutate(
    in_treatment_window = case_when(
      `Time (start hour)`>=tstart & `Time (start hour)`<tend ~ 1, 
      .default = 0)) %>% 
  select(park_name, branch_code, stage, strata, route_code, route_name, 
         treatment_window, `Time (start hour)`, in_treatment_window,
         `Observed average frequency (oaf)`, iqr_oaf, p25_oaf, p50_oaf, p75_oaf)


# Calculate hourly payments 
  #  ...


# Save 
write_csv(route_hourly, 
          file.path(git_dir, "data", phase, "route_hourly.csv"))

 

# Create stage_strata dataset --------------------------------------------------------
stage_strata <- route_roster %>% 
  distinct(taxi_park, stage, branch_code, strata) %>% 
  arrange(strata, taxi_park, branch_code)

write_csv(stage_strata, 
          file.path(git_dir, "data", phase, "stage_strata.csv"))









# Load guide form data -----------------------------------------------------------
clean_path <- "G:/Shared drives/ugandatransit/uganda_transit_archives/dataprocessed/frequency-intervention-otp/guide-form/phase2"
load(file.path(clean_path, "guideformdatasets_clean.RDS"), verbose = TRUE)


# List of route names to sum across 
routecols <- guideform_corrected %>% ungroup() %>% select(starts_with("taxi_departed_routes_")) 
routenames <- which(colSums(routecols, na.rm=TRUE)!=0) %>% 
  names() %>% str_remove_all("taxi_departed_routes_")


