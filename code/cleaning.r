
library(tidyverse)
library(stargazer)

## -- Load Data --------------------------------------------------------------------------
data_perf <- read_dta("data/performance_labeled.dta")
data_attitude <- read_dta("data/attitude_labeled.dta")
data_endperiod <- read_dta("data/endperiod_outcomes_labeled.dta")
data_sum_vol <- read_dta("data/summary_volunteer_labeled.dta")
data_wage_new <- read_dta("data/wage_new_labeled.dta")

## -- Tibbles ----------------------------------------------------------------------------
tb.perf <- as_tibble(data_perf)
tb.attitude <- as_tibble(data_attitude)
tb.endperiod <- as_tibble(data_endperiod)
tb.sum_vul <- as_tibble(data_sum_vol)
tb.wage <- as_tibble(data_wage_new)

rm(data_perf)
rm(data_attitude)
rm(data_endperiod)
rm(data_sum_vol)
rm(data_wage_new)
