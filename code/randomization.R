library(tidyverse)
library(here)

set.seed(4982392)

strata <- here("data",
               "strata.csv") |>
  read_csv()

a_strata <- strata |>
  filter(stratum == "A")

b_strata <- strata |>
  filter(stratum == "B")

a_strata_treated <- sample(a_strata$stage,
                           size = nrow(a_strata)/2)

b_strata_treated <- sample(b_strata$stage,
                           size = nrow(b_strata)/2)

treated <- c(a_strata_treated, b_strata_treated)

treatment_groups <- strata |> 
  mutate(group = ifelse(stage %in% treated, "Treatment", "Control"))

write_csv(treatment_groups,
          here("data",
               "treatment_groups.csv"))
