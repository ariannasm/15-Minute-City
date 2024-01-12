

## INPUTS
## -----------
## outcomes_urb_clean_popweighted.csv
## outcomes_blckgrp_clean_popweighted.csv

## OUTPUTS
## -----------
## TableS4.tex

## ==========================
## 1. Loading Libraries
## ==========================

library(lfe)
library(tidyverse)
library(stargazer) 
library(corrplot)
library(haven)
library(fixest)


## ==========================
## Set Main Path (User should modify this only)
## ==========================
main_path <- "path/to/replication"  # User should replace this with the path to the 'replication' folder


# ==========================
## 2. Load Data
## ==========================

# Set working directory to main path
setwd(main_path)

# Read the datasets.
data_path <- file.path(main_path, "data", "clean", "outcomes_blckgrp_clean_allpois_popweighted.csv")
data_urb_path <- file.path(main_path, "data", "clean", "outcomes_urb_clean_allpois_popweighted.csv")

data <- read.csv(data_path)
data_urb <- read.csv(data_urb_path)


# Rename variables
data  <- data %>% dplyr::rename(
  density_hu_acre = epa_D1A,
  total_pop = idx_total_population,
  density_pop_acre = epa_D1B,
  median_hh_income = cs_b19013e1,
  density_road = epa_D3A
)

data_urb  <- data_urb %>% dplyr::rename(
  density_hu_acre = epa_D1A,
  total_pop = idx_total_population,
  density_pop_acre = epa_D1B,
  median_hh_income = cs_b19013e1,
  density_road = epa_D3A
)

# Compute percent with education BA+
data$pct_ed_ba_plus <- 100 * (data$cs_b15003e22 + data$cs_b15003e23 + data$cs_b15003e24 + data$cs_b15003e25) / data$cs_b15003e1
data_urb$pct_ed_ba_plus <- 100 * (data_urb$cs_b15003e22 + data_urb$cs_b15003e23 + data_urb$cs_b15003e24 + data_urb$cs_b15003e25) / data_urb$cs_b15003e1


# Compute the median usage weighted by CBG population
round(reldist::wtd.quantile(data$usage_pct_all_cats_home_bg_based, 0.5, weight = data$total_pop),2)
round(modi::weighted.quantile(data$usage_pct_all_cats_home_bg_based, data$total_pop, prob = 0.5),2)
round(sum(data$usage_pct_all_cats_home_bg_based<=0.5)/nrow(data),2)
n_distinct(data$urb)


# Format variables (CBG-level data)
data$city <- as.factor(data$urb)
data$bg_geoid <- as.factor(data$bg_geoid)
data$census_tract <- as.factor(substr(data$bg_geoid,1,11))
data$county_id <- as.factor(substr(data$bg_geoid,1,5))
data <- data %>% dplyr::rename(epa_transit_freq_sqm=epa_D4D)

# Create additional transformations, i.e. percentages and logs for controls (CBG-level)
data <- data %>% mutate(
  median_hh_income_pct = percent_rank(median_hh_income),
  pct_white = 100 * cs_share_white,
  pop_density_log = log1p(density_pop_acre),
  hu_density_log = log1p(density_hu_acre),
  transit_stops_density_log = log1p(doc_transit_density_acre),
  median_income_log = log1p(median_hh_income)
)


# Format variables (URB-level data)
data_urb$city <- as.factor(data_urb$urb)
data_urb <- data_urb %>% dplyr::rename(epa_transit_freq_sqm=epa_D4D)

# Create additional transformations, i.e. percentages and logs for controls (URB-level)
data_urb <- data_urb %>% mutate(
  median_hh_income_pct = percent_rank(median_hh_income),
  pct_white = 100 * cs_share_white,
  pop_density_log = log1p(density_pop_acre),
  hu_density_log = log1p(density_hu_acre),
  transit_stops_density_log = log1p(replace_na(doc_transit_density_acre,0)),
  median_income_log = log1p(median_hh_income)
)


# Normalize outcomes
data$usage_pct_all_cats_home_bg_based <- 100 * data$usage_pct_all_cats_home_bg_based
data$access_idx_crosscity_wt <- 100 * data$access_idx_crosscity_wt
data_urb$usage_pct_all_cats_home_bg_based <- 100 * data_urb$usage_pct_all_cats_home_bg_based
data_urb$access_idx_crosscity_wt <- 100 * data_urb$access_idx_crosscity_wt

## ==========================
## 4. Regression Results
## ==========================

# Run main regressions, first without controls then two more, adding controls  (CBG level)
est_c0 <- felm(usage_pct_all_cats_home_bg_based ~ access_idx_crosscity_wt | city | 0 | county, data=data)
est_c1 <- felm(usage_pct_all_cats_home_bg_based ~ access_idx_crosscity_wt + pop_density_log + median_income_log + pct_ed_ba_plus + pct_white| city | 0 | county, data=data)
est_c4 <- felm(usage_pct_all_cats_home_bg_based ~ access_idx_crosscity_wt + pop_density_log + median_income_log + pct_ed_ba_plus + pct_white + epa_transit_freq_sqm + avg_dist_work| city | 0 | county, data=data)

# Run main regressions, first without controls then two more, adding controls  (URB level)
est_ur0 <- felm(usage_pct_all_cats_home_bg_based ~ access_idx_crosscity_wt, data=data_urb)
est_ur1 <- felm(usage_pct_all_cats_home_bg_based ~ access_idx_crosscity_wt + pop_density_log + median_income_log + pct_ed_ba_plus + pct_white, data=data_urb)
est_ur4 <- felm(usage_pct_all_cats_home_bg_based ~ access_idx_crosscity_wt + pop_density_log + median_income_log + pct_ed_ba_plus + pct_white + epa_transit_freq_sqm + avg_dist_work, data=data_urb)


# Create the table with regression results
table_str <- stargazer(est_c0, est_c1, est_c4, est_ur0, est_ur1, est_ur4,
                       type='latex',
                       report=('vc*p'),
                       covariate.labels = c(
                         "15-minute Access",
                         "Pop Density Log",
                         "Median Income Log",
                         "\\% Education: BA +",
                         "\\% White",
                         "Transit Frequency (per sq. mi)",
                         "Average Commute (mi)"
                       ),
                       add.lines=list(c("Urban Area FE", "\\checkmark", "\\checkmark", "\\checkmark","", "", "")),
                       dep.var.labels = c("15-minute Usage", "15-minute Access"),
                       column.labels = c("Across CBGs", "Across Urban Areas"),
                       omit.stat=c("f", "ser", "adj.rsq"),
                       omit=c("Constant"),
                       column.separate=c(4,4),
                       dep.var.caption=c("Dependent Variable (between 0 and 100)")
)


# Print the table with desired p-value format
write(gsub("p = 0.000[0-9]*", "p $<$ 0.001",paste(table_str,collapse="\n")), file="output/TableS4.tex")

# Output excludes partial R^2. See script "tableS4_partial_rsquared". 


