## INPUTS
## -----------
## outcomes_blckgrp_clean_popweighted.csv
## cbgs_nyc_historical_zoning_data.csv

## OUTPUTS
## -----------
## Figure5.pdf

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
packages <- c("lfe", "tidyverse", "stargazer", "corrplot", "haven", "fixest", "GGally", "stringr", "ggpubr", "dplyr")

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

# Define paths to data files
cbgs_data_path <- file.path(main_path, "data", "clean", "outcomes_blckgrp_clean_popweighted.csv")
hist_nyc_data_path <- file.path(main_path, "data", "clean", "cbgs_nyc_historical_zoning_data.csv")


# Read the datasets.
data <- read.csv(cbgs_data_path)
cbgs_nyc_historical_zoning_data <- read.csv(hist_nyc_data_path)

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

## ==========================
## 5. Visualization Preparation
## ==========================

# Run regressions (IV1)
felm_iv1 <- felm(usage_percent ~ hc_pop_den_log + hc_hu_den_log + poly(cbd_distance_miles,3) +
                 hc_workers_textile_apparel + hc_workers_eat_drink + hc_workers_other_retail + 
                 hc_workers_other_service + hc_workers_education + hc_workers_other_mfg 
                 | county | (access_percent ~ log_FAR) | census_tract, data=data)
                 
# Substitute model names for table presentation
rownames(felm_iv1$coefficients) <- sub('.*access_percent.*', "access_percent", rownames(felm_iv1$coefficients))
rownames(felm_iv1$beta) <- sub('.*access_percent.*', "access_percent", rownames(felm_iv1$beta))

## Plot the first stage regression line (IV1)
fig_first_stage <- ggplot(data, aes(x=zh_FAR, y=access_percent)) +
  geom_point(color="grey", size=1, alpha =0.7, stroke = 0) +
  geom_smooth(method = lm,  formula=y~log(x), color='black', lwd=1, show.legend=TRUE, na.rm = TRUE, fill='red') +
  guides(size="none", shape=guide_legend(title=element_blank()), color="none") +
  geom_text(x=9, y=63, label=paste(
      "First stage F-stat =",
      round(as.numeric(felm_iv1$stage1$iv1fstat$access_percent)[5],digits=2)
  ), family='Helvetica', check_overlap = T) + 
  labs(
        y = "15-minute Access (2019)",
        x = "Average FAR in commercial districts (1961 zoning)",
    ) + 
  theme_bw() +
  ylim(60,105) +   
  theme(
              axis.text.y = element_text(size=9),
              axis.text.x = element_text(size=9),
              plot.margin = margin(
                  t = 15, r = 20, b = 15, l = 30,
                  unit = "points")
           )


## Plot the reduced form relation 
fig_first_stage_usage <- ggplot(data, aes(x=zh_FAR, y=usage_percent)) +
  geom_point(color="grey",  size=1, alpha =0.7, stroke = 0) + 
  geom_smooth(method = lm,  formula=y~log(x), color='black', lwd=1, show.legend=TRUE, na.rm = TRUE) +
  guides(size="none", shape=guide_legend(title=element_blank()), color="none") + labs(
        y = "15-minute Usage (% trips) (2019)",
        x = "Average FAR in commercial districts (1961 zoning)",
    ) + 
  theme_bw() +
  ylim(0,100) +   
  theme(
              text=element_text(size=11,  family="Helvetica"),
              axis.text.y = element_text(size=9),
              axis.text.x = element_text(size=9),
              plot.margin = margin(
                  t = 15, r = 30, b = 15, l = 20, 
                  unit = "points")
           )


# Assemble figures and export
pdf(file = "output/figure_5.pdf", width = 8, height = 3)
ggarrange(fig_first_stage,fig_first_stage_usage,
                    labels = c("a", "b"),
                    ncol = 2, nrow = 1, widths=c(0.5,0.5), align='h'
         )
dev.off()

