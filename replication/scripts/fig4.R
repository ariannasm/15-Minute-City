## INPUTS
## -----------
## outcomes_blckgrp_clean_popweighted.csv
## outcomes_urb_clean_popweighted.csv


## OUTPUTS
## -----------
## Figure4.pdf

## ==========================
## Set Main Path (User should modify this only)
## ==========================
main_path <- "path/to/replication"  # User should replace this with the path to the 'replication' folder

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
packages <- c("lfe", "tidyverse", "stargazer", "haven", "fixest", "tmap", 
              "sf", "rosm", "prettymapr", "ggpubr", "tmaptools", "grid", 
              "gridExtra", "osmdata", "reshape2", "repr", "stringr", "ggrepel", "OpenStreetMap", "ggtext")

# Apply the function to each package
invisible(sapply(packages, install_and_load))

# Install OpenStreetMap
if (!require(devtools)) install.packages("devtools")
devtools::install_github("ifellows/ROSM/OpenStreetMap")


## ==========================
## 2. Load Data
## ==========================

# Set the working directory to the desired path.
setwd(main_path)

# Define paths to data files
cbgs_data_path <- file.path(main_path, "data", "clean", "outcomes_blckgrp_clean_popweighted.csv")
urban_areas_data_path <- file.path(main_path, "data", "clean", "outcomes_urb_clean_popweighted.csv")

# Read the datasets.
cbgs <- read.csv(cbgs_data_path)
urban_areas <- read.csv(urban_areas_data_path)


## ==========================
## 3. Data Processing
## ==========================

# Reading the dataset of block groups and renaming columns
cbgs  <- cbgs %>% dplyr::rename(
  density_hu_acre = epa_D1A,
  total_pop = idx_total_population,
  density_pop_acre = epa_D1B,
  median_hh_income = cs_b19013e1,
  density_road = epa_D3A
)

# Calculating percentage education (BA and above) for cbgs. 
cbgs$pct_ed_ba_plus <- 100 * (cbgs$cs_b15003e22 + cbgs$cs_b15003e23 + cbgs$cs_b15003e24 + cbgs$cs_b15003e25) / cbgs$cs_b15003e1

# Reading the dataset of urban areas and renaming columns
urban_areas  <- urban_areas %>% dplyr::rename(
  density_hu_acre = epa_D1A,
  total_pop = cs_b01001e1_urb,
  density_pop_acre = epa_D1B,
  median_hh_income = cs_b19013e1,
  density_road = epa_D3A
)

# Calculate the percentage of individuals with education (BA and above) for urban areas.
urban_areas$pct_ed_ba_plus <- 100 * (urban_areas$cs_b15003e22 + urban_areas$cs_b15003e23 + urban_areas$cs_b15003e24 + urban_areas$cs_b15003e25) / urban_areas$cs_b15003e1


# Extract urban area and state codes from the urban area name.
urban_areas$Urban.Area  <- as.character(urban_areas$urb_name) %>% purrr::map(function (n) toString(
  unlist(strsplit(strsplit(n,", ")[[1]][[1]],"--")[[1]][[1]])
))

urban_areas$State.Code <- as.character(urban_areas$urb_name) %>% purrr::map(function (n) toString(
  unlist(strsplit(strsplit(n,", ")[[1]][[-1]],"--")[[1]][[1]])
))  

urban_areas$State.Code <- as.character(urban_areas$State.Code)
urban_areas$State.Code <- as.factor(urban_areas$State.Code)
urban_areas$ua <- str_c(urban_areas$Urban.Area, urban_areas$State.Code, sep=", ")


# Join urban area data to block groups dataset and extract state codes from urban area name.
cbgs <- cbgs %>% left_join(urban_areas %>% dplyr::select(urb,Urban.Area))
cbgs$State.Code <- as.character(cbgs$urb_name) %>% purrr::map(function (n) toString(
  unlist(strsplit(strsplit(n,", ")[[1]][[-1]],"--")[[1]][[1]])
))      

cbgs$State.Code <- as.character(cbgs$State.Code)
cbgs$State.Code <- as.factor(cbgs$State.Code)
cbgs$ua <- str_c(cbgs$Urban.Area, cbgs$State.Code, sep=", ")                                                                    


## ==========================
## 4. Normalize Measures
## ==========================

# Multiply usage percentages and accessibility indexes by 100 to convert to percentage format.
cbgs$usage_pct_all_cats_home_bg_based <- 100 * cbgs$usage_pct_all_cats_home_bg_based
cbgs$access_idx_crosscity_wt <- 100 * cbgs$access_idx_crosscity_wt
urban_areas$usage_pct_all_cats_home_bg_based <- 100 * urban_areas$usage_pct_all_cats_home_bg_based
urban_areas$access_idx_crosscity_wt <- 100 * urban_areas$access_idx_crosscity_wt


## ==========================
## 5. Visualization
## ==========================

# Create a list of urban areas to be highlighted
msas <- c(
    'Los Angeles--Long Beach--Anaheim, CA',
    'Detroit, MI',
    'New York--Newark, NY--NJ--CT',
    'Atlanta, GA',
    'Portland, OR--WA',
    'Seattle, WA'
)


# Create a column to indicate which highlight is used for each city

cbgs$plot_group <- if_else((cbgs$urb_name %in% msas), "--", as.character(cbgs$urb_name), "missing")
urban_areas$plot_group <- dplyr::if_else(!as.logical(urban_areas$urb_name %in% msas), c("--"), as.character(urban_areas$urb_name), "missing")


# Create log transformed variables for population size visualization.

urban_areas$POP <- urban_areas$total_pop
urban_areas$pop_log <- log(urban_areas$POP)


# Change variable names
cbgs$NAME <- cbgs$urb_name
urban_areas$NAME <- urban_areas$urb_name


# Arrange the rows in the order of hihlighting
urban_areas <- urban_areas %>% arrange(desc(plot_group),desc(POP))


# List of urban areas to be annotated
select_msas = c(
    'Reading, PA',
    'New York--Newark, NY--NJ--CT',
    'Boston, MA--NH--RI',
    'Seattle, WA',
    'Atlanta, GA',
    'San Francisco--Oakland, CA',
    'Detroit, MI',
    'Philadelphia, PA--NJ--DE--MD',
    'Portland, OR--WA',
    'Cleveland, OH',
    'Miami, FL',
    'San Francisco--Oakland, CA',
    'Los Angeles--Long Beach--Anaheim, CA',
    'Phoenix--Mesa, AZ',
    'Avondale--Goodyear, AZ'
)


# Plotting access and usage across urban areas

usage_access_msas_scatter <- ggplot(urban_areas, aes(access_idx_crosscity_wt, usage_pct_all_cats_home_bg_based)) +
  geom_point(color='grey',aes(size = pop_log, shape = factor(Region)), show.legend=FALSE, alpha=0.7) +
  geom_point(aes(size = pop_log, shape = factor(Region), color=factor(plot_group)),
             data = filter(urban_areas, NAME %in% select_msas),
             show.legend=TRUE) +
  geom_smooth(method = "lm", se=FALSE, color='black', lwd=0.5) +
  stat_regline_equation(label.y = 47.5, aes(label = ..rr.label..)) +
  guides(size="none", shape=guide_legend(title=element_blank()), color="none") +
  scale_size_continuous(range = c(0.25, 3)) +
  geom_label_repel(min.segment.length = 0, max.overlaps = Inf,
                   alpha = 0.7, 
                   label.padding = 0.2,  show.legend=FALSE,
                   size=2.5,
                   seed = 1234,
                   label.size = NA,
                   aes(
                       family='Helvetica',
                       label = (filter(urban_areas, NAME %in% select_msas))$Urban.Area,
                       color=factor(plot_group)),
                   data = filter(urban_areas, NAME %in% select_msas)
                  ) +
  geom_label_repel(min.segment.length = 0, max.overlaps = Inf,
                    alpha = 1, 
                    label.padding = 0.2,  show.legend=FALSE,
                    size=2.5,
                    fill = NA,
                    label.size = NA,
                    seed = 1234,
                    aes(
                       family='Helvetica',
                       label = (filter(urban_areas, NAME %in% select_msas))$Urban.Area,
                       color=factor(plot_group)),
                    data = filter(urban_areas, NAME %in% select_msas)
                    ) +
  scale_color_manual(values=c("#4a4a4a", "#9a024e",  "#f25d46", "#e4ca23","#075db4","#096f53", "#d8abe7")) +
  scale_shape_manual(values=c(15,16,17,18)) + 
  labs(
        x = "15-minute Access (# amenities, ranked)",
        y = "15-minute Usage (% trips)",
    ) + 
  theme_bw() +
  theme(legend.position="top",
          legend.justification="right",
          legend.direction = "horizontal",
          legend.background = element_rect(fill=alpha('black', 0)),
          legend.title.align=0.5,
          legend.box.just = c("bottom"),
          legend.margin=margin(0,0,-10,-10),
          text=element_text(size=11,  family="Helvetica"),
          legend.title = element_text(color = '#666666', size=11),
          legend.spacing.x = unit(5, 'points'),
          axis.text.y = element_text(size=9),
          axis.text.x = element_text(size=9),
          plot.margin = margin(
              t = 5, r = 10, b = 10, l = 6, 
              unit = "points")
       )


# Plotting usage and access within urban areas

usage_access_cbgs_scatter <- ggplot(
       cbgs, aes(access_idx_crosscity_wt, usage_pct_all_cats_home_bg_based)
    ) +
    geom_point(color='#ededed', data = filter(cbgs, !NAME %in% msas), show.legend=FALSE, alpha=0.8, size=0.35, shape=4) +
    geom_point(aes(color=ua), data = filter(cbgs, NAME %in% msas) %>% group_by(NAME) %>% slice_sample(n = 100),
               show.legend=FALSE, alpha=0.8, size=0.35, shape=4) +
    geom_smooth(method = lm,
                aes(color = ua),
                formula = y ~ splines::bs(x, 3),
                se = FALSE, size=0.8,
                show.legend=FALSE,
                data= cbgs %>% filter(NAME %in% msas)) +
    # stat_regline_equation(label.y = 80, aes(label = ..rr.label..)) +
    geom_richtext(x=25, y=80,
              label= "_R_<sup>2</sup> = 0.74",
              family='Helvetica', 
              fill = NA, label.color = NA, 
              data = data.frame(x=c(20),y=c(80))) + 
    scale_color_manual(values=c("#e4ca23", "#9a024e", "#f25d46" ,"#075db4","#096f53", "#d8abe7")) +
    guides(color=guide_legend(title=element_blank())) +
    theme_bw() + 
    labs(
            x = "15-minute Access",
            y = "15-minute Usage",
        ) + 
      theme_bw() +
      ylim(0, 80) + 
      theme(
              text=element_text(size=11,  family="Helvetica"),
              legend.title = element_text(color = '#666666', size=11),
              axis.text.y = element_text(size=9),
              axis.text.x = element_text(size=9),
              plot.margin = margin(
                  t = 23, r = 10, b = 10, l = 10, 
                  unit = "points")
           )


# Assemble figures and export

pdf(file = "output/figure_4.pdf", width = 9, height = 3.5)
figure_usage_acccess <- ggarrange(usage_access_msas_scatter,usage_access_cbgs_scatter,
          nrow = 1,
          labels = c("a","b")               
          ) 
figure_usage_acccess
dev.off()

