library(tidyverse)

abs_district_ap <- read_csv("N:/ORP_accountability/projects/2020_amo/absenteeism_targets_district.csv")
abs_district_am <- read_csv("N:/ORP_accountability/projects/2020_amo/absenteeism_targets_district_AM.csv")

setdiff(abs_district_ap, abs_district_am)
setdiff(abs_district_am, abs_district_ap)

abs_school_ap <- read_csv("N:/ORP_accountability/projects/2020_amo/absenteeism_targets_school.csv")
abs_school_am <- read_csv("N:/ORP_accountability/projects/2020_amo/absenteeism_targets_school_AM.csv")

setdiff(abs_school_ap, abs_school_am)
setdiff(abs_school_am, abs_school_ap)

elpa_district_ap <- read_csv("N:/ORP_accountability/projects/2020_amo/elpa_targets_district.csv")
elpa_district_am <- read_csv("N:/ORP_accountability/projects/2020_amo/elpa_targets_district_AM.csv")

setdiff(elpa_district_ap, elpa_district_am)
setdiff(elpa_district_am, elpa_district_ap)

elpa_school_ap <- read_csv("N:/ORP_accountability/projects/2020_amo/elpa_targets_school.csv")
elpa_school_am <- read_csv("N:/ORP_accountability/projects/2020_amo/elpa_targets_school_AM.csv") %>%
    select(-system_name, -school_name)

setdiff(elpa_school_ap, elpa_school_am)
setdiff(elpa_school_am, elpa_school_ap)

grad_district_ap <- read_csv("N:/ORP_accountability/projects/2020_amo/grad_targets_district.csv")
grad_district_am <- read_csv("N:/ORP_accountability/projects/2020_amo/grad_targets_district_AM.csv")

setdiff(grad_district_ap, grad_district_am)
setdiff(grad_district_am, grad_district_ap)

grad_school_ap <- read_csv("N:/ORP_accountability/projects/2020_amo/grad_targets_school.csv")
grad_school_am <- read_csv("N:/ORP_accountability/projects/2020_amo/grad_targets_school_AM.csv")

setdiff(grad_school_ap, grad_school_am)
setdiff(grad_school_am, grad_school_ap)

ready_ap <- read_csv("N:/ORP_accountability/projects/2020_amo/ready_grad_targets_school.csv")
ready_am <- read_csv("N:/ORP_accountability/projects/2020_amo/ready_grad_targets_school_AM.csv") %>%
    rename(pct_ready_grad_prior = pct_ready_grad)

setdiff(ready_ap, ready_am)
setdiff(ready_am, ready_ap)

success_school_ap <- read_csv("N:/ORP_accountability/projects/2020_amo/success_rate_targets_school.csv")
success_school_am <- read_csv("N:/ORP_accountability/projects/2020_amo/success_rate_targets_school_AM.csv") %>%
    select(-system_name, -school_name)

setdiff(success_school_ap, success_school_am)
setdiff(success_school_am, success_school_ap)

success_district_ap <- read_csv("N:/ORP_accountability/projects/2020_amo/success_rate_targets_district.csv")
success_district_am <- read_csv("N:/ORP_accountability/projects/2020_amo/success_rate_targets_district_AM.csv") %>%
    select(-system_name)

setdiff(success_district_ap, success_district_am)
setdiff(success_district_am, success_district_ap)

