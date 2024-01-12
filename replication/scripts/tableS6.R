## INPUTS
## -----------
## outcomes_urb_clean_popweighted.csv
## cbgs_nyc_historical_zoning_data.csv

## OUTPUTS
## -----------
## TableS6.tex

## ==========================
## 1. Loading Libraries
## ==========================

# Function to check and install any missing packages
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    cat(sprintf("Installing package: '%s'\n", package))
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  } else {
    cat(sprintf("Package '%s' is already installed.\n", package))
  }
}

# List of required packages
packages <- c("lfe", "ggplot2", "tidyverse", "stargazer", "fixest", "GGally", 
              "stringr", "ggpubr")

# Apply the function to each package
invisible(sapply(packages, install_and_load))


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
data_path <- file.path(main_path, "data", "clean", "outcomes_blckgrp_clean_popweighted.csv")
hist_path <- file.path(main_path, "data", "clean", "cbgs_nyc_historical_zoning_data.csv")

data <- read.csv(data_path)
cbgs_nyc_historical_zoning_data <- read.csv(hist_path)


## ==========================
## 3. Data Transformation
## ==========================


# Rename variables
data  <- data %>% dplyr::rename(
  density_hu_acre = epa_D1A,
  total_pop = idx_total_population,
  density_pop_acre = epa_D1B,
  median_hh_income = cs_b19013e1,
  density_road = epa_D3A
)


# Compute percent with education BA+
data$pct_ed_ba_plus <- 100 * (data$cs_b15003e22 + data$cs_b15003e23 + data$cs_b15003e24 + data$cs_b15003e25) / data$cs_b15003e1


# Join datasets
data <- data %>% inner_join(cbgs_nyc_historical_zoning_data, by='bg_geoid')

# Normalize outcomes
data$usage_percent <- 100 * data$usage_pct_all_cats_home_bg_based 
data$access_percent <- 100 * data$access_idx_crosscity_wt 

# Format variables, take log transformations for controls
data$city <- as.factor(data$urb)
data$bg_geoid <- as.factor(data$bg_geoid)
data$census_tract <- as.factor(substr(data$bg_geoid,1,11))
data$county_id <- as.factor(substr(data$bg_geoid,1,5))
data <- data %>% mutate(
  pct_white = 100 * cs_share_white,
  pop_density_log = log1p(density_pop_acre),
  hu_density_log = log1p(density_hu_acre),
  median_income_log = log1p(median_hh_income)
)

# Compute log FAR (floor-to-area)
data$log_FAR <- log(data$zh_FAR)

# Format variables, take log transformations for historical controls
data <- mutate(data, 
               hc_pop_den_log = log1p(hc_pop_total / area_acres),
               hc_hu_den_log = log1p(hc_hu_total / area_acres),
               hc_pop_den = (hc_pop_total / area_acres),
               hc_hu_den = (hc_hu_total / area_acres),
               hc_pct_families_nw = 100 * (hc_families_nw_total / hc_families_total),
               hc_pct_families_income_below_1k = 100 * (hc_families_income_below_1k / hc_families_total),
               hc_pct_families_nw_income_below_1k = 100 * (hc_families_nw_income_below_1k / hc_families_income_below_1k),
)


# Create a rank variable from the raw stayers variable
data <- mutate(data, stayers_rank = as.factor(ntile(data$oi_kid_stay_share,4)))


## ==========================
## 4. Regression Results
## ==========================


# Only keep the top quartile of census tracts by share of stayers
data_robust_temp <- data %>% filter(stayers_rank==4)


# Run regressions (OLS, IV1, IV2, IV3)
felm_ols_r <- felm(usage_percent ~ access_percent + pop_density_log + median_income_log + poly(cbd_distance_miles,3) + hc_pop_den_log + hc_hu_den_log + 
                 hc_workers_textile_apparel + hc_workers_eat_drink + hc_workers_other_retail + 
                 hc_workers_other_service + hc_workers_education + hc_workers_other_mfg 
                 | county,  data=data_robust_temp)

felm_iv1_r <- felm(usage_percent ~ hc_pop_den_log + hc_hu_den_log + poly(cbd_distance_miles,3) +
                 hc_workers_textile_apparel + hc_workers_eat_drink + hc_workers_other_retail + 
                 hc_workers_other_service + hc_workers_education + hc_workers_other_mfg 
                 | county | (access_percent ~ log_FAR) | census_tract, data=data_robust_temp)

felm_iv2_r <- felm(usage_percent ~ pop_density_log + median_income_log + hc_pop_den_log + hc_hu_den_log + poly(cbd_distance_miles,3) +
                 hc_workers_textile_apparel + hc_workers_eat_drink + hc_workers_other_retail + 
                 hc_workers_other_service + hc_workers_education + hc_workers_other_mfg 
                 | county | (access_percent ~ log_FAR) | census_tract, data=data_robust_temp)

felm_iv3_r <- felm(usage_percent ~ pop_density_log + median_income_log + hc_pop_den * hc_pct_families_income_below_1k + hc_hu_den_log + poly(cbd_distance_miles,3) +
                 hc_workers_textile_apparel + hc_workers_eat_drink + hc_workers_other_retail + 
                 hc_workers_other_service + hc_workers_education + hc_workers_other_mfg + 
                 hc_pct_families_income_below_1k + hc_pct_families_nw * hc_pct_families_income_below_1k | county | (access_percent ~ log_FAR) | census_tract, data=data_robust_temp)

# Substitute model names for table presentation

rownames(felm_iv1_r$coefficients) <- sub('.*access_percent.*', "access_percent", rownames(felm_iv1_r$coefficients))
rownames(felm_iv1_r$beta) <- sub('.*access_percent.*', "access_percent", rownames(felm_iv1_r$beta))
rownames(felm_iv2_r$coefficients) <- sub('.*access_percent.*', "access_percent", rownames(felm_iv2_r$coefficients))
rownames(felm_iv2_r$beta) <- sub('.*access_percent.*', "access_percent", rownames(felm_iv2_r$beta))
rownames(felm_iv3_r$coefficients) <- sub('.*access_percent.*', "access_percent", rownames(felm_iv3_r$coefficients))
rownames(felm_iv3_r$beta) <- sub('.*access_percent.*', "access_percent", rownames(felm_iv3_r$beta))

# Create the table with regression results
table_str <- stargazer(felm_ols_r, felm_iv1_r, felm_iv2_r, felm_iv3_r, type='latex',
          report=('vc*p'), omit=c("hc_pop*", "hc_hu*", "hc_workers*", "hc_pct_families*","poly*"),
          covariate.labels=c("Access to amenities within a 15m walk", 
                             "Population Density (Log)",
                             "Median Income (Log)"),
          column.separate=c(1,3),
          column.labels=c('OLS','IV'),
          omit.stat=c("ser"),
        add.lines=list(c("IV: Log Average Commercial FAR in 1961 Resolution"," ","\\checkmark","\\checkmark","\\checkmark"),
                        c("Distance to City Center", "\\checkmark","\\checkmark", "\\checkmark", "\\checkmark"),
                        c("1960 Population and Housing Density", "\\checkmark","\\checkmark", "\\checkmark", "\\checkmark"),
                        c("1960 Workers by Industry", "\\checkmark","\\checkmark", "\\checkmark", "\\checkmark"),
                        c("1960 Poor and Non-white Families", " "," ", " ", "\\checkmark"),
                        c("First stage F-stat","NA",
                          round(as.numeric(felm_iv1_r$stage1$iv1fstat$access_percent)[5],digits=2),
                          round(as.numeric(felm_iv2_r$stage1$iv1fstat$access_percent)[5],digits=2),
                          round(as.numeric(felm_iv3_r$stage1$iv1fstat$access_percent)[5],digits=2)
                         )
                       ),
          dep.var.labels = c("Usage (\\% of trips within a 15m walk)")
         )


# Print the table with desired p-value format
write(gsub("p = 0.000[0-9]*", "p $<$ 0.001",paste(table_str,collapse="\n")), file="output/TableS6.tex")



