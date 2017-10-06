library(acct)
library(tidyverse)

elpa <- read_csv("K:/ORP_accountability/data/2017_ELPA/system_elpa.csv") %>%
    mutate(AMO_target = amo_target(valid_tests, pct_met_growth, n_minimum = 10),
        AMO_target_4 = amo_target(valid_tests, pct_met_growth, double = TRUE, n_minimum = 10))

write_csv(elpa, path = "K:/ORP_accountability/projects/2018_amo/district_elpa.csv", na = "")
