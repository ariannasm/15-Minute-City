## INPUTS
## -----------
## sg_cbp_businesses_by_county.csv

## OUTPUTS
## -----------
## TableS8.tex

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
packages <- c("lfe", "tidyverse", "stargazer", "fixest")

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
data_path <- file.path(main_path, "data", "clean", "sg_cbp_businesses_by_county.csv")
sg_cbp <- read.csv(data_path) 

## ==========================
## 3. Regression Results
## ==========================

# Run simple OLS
fit_simple <- lm(formula = count_cbp ~ count_sg, data = sg_cbp)

# Run OLS with NAICS-level fixed effects
fit_fe1 <- lm(formula = count_cbp ~ count_sg + naics_factor, data = sg_cbp)

# Create the table with regression results
table_str <- stargazer(
    fit_simple, fit_fe1,
    report=('vc*p'),
    add.lines = list(c("2-Digit NAICS Code FE","No","Yes")),
    dep.var.caption = "All County x Industry Pairs (NAICS 2-digit)",
    omit.stat=c("ser","adj.rsq"),
    omit=c("naics*"),
    covariate.labels = c("Establishments Count - Safegraph"),
    dep.var.labels=c("Establishments Count - Census CBP"),
    type='latex')


# Print table with desired formatting
write(gsub("p = 0.000[0-9]*", "p $<$ 0.001",paste(table_str,collapse="\n")), file="output/TableS8.tex")



