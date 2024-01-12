## INPUTS
## -----------
## outcomes_blckgrp_clean_popweighted.csv
## outcomes_urb_clean_popweighted.csv
## dummy_safegraph_data.csv

## OUTPUTS
## -----------
## Figure1.pdf

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
              "gridExtra", "osmdata", "reshape2", "repr", "stringr", "ggrepel", "OpenStreetMap")

# Apply the function to each package
invisible(sapply(packages, install_and_load))

# Install OpenStreetMap
if (!require(devtools)) install.packages("devtools")
devtools::install_github("ifellows/ROSM/OpenStreetMap")


## ==========================
## 2. Data Preparation
## ==========================

# Set working directory to main path
setwd(main_path)

# Define paths to data files
cbgs_data_path <- file.path(main_path, "data", "clean", "outcomes_blckgrp_clean_popweighted.csv")
urban_areas_data_path <- file.path(main_path, "data", "clean", "outcomes_urb_clean_popweighted.csv")
iso_data_path <- file.path(main_path, "data", "clean", "dummy_safegraph_data.csv")
urban_areas_geo_path <- file.path(main_path, "data", "raw", "census_urbanareas_2021", "tl_2021_us_uac10", "tl_2021_us_uac10.shp")


# Reading the dataset of block groups and renaming columns
cbgs <- read.csv(cbgs_data_path)

cbgs  <- cbgs %>% dplyr::rename(
  density_hu_acre = epa_D1A,
  total_pop = idx_total_population,
  density_pop_acre = epa_D1B,
  median_hh_income = cs_b19013e1,
  density_road = epa_D3A
)

# Calculating percentage education (BA and above)
cbgs$pct_ed_ba_plus <- 100 * (cbgs$cs_b15003e22 + cbgs$cs_b15003e23 + cbgs$cs_b15003e24 + cbgs$cs_b15003e25) / cbgs$cs_b15003e1


# Reading urban areas data and renaming columns
urban_areas <- read.csv(urban_areas_data_path)
urban_areas  <- urban_areas %>% dplyr::rename(
  density_hu_acre = epa_D1A,
  total_pop = idx_total_population,
  density_pop_acre = epa_D1B,
  median_hh_income = cs_b19013e1,
  density_road = epa_D3A
)

urban_areas$pct_ed_ba_plus <- 100 * (urban_areas$cs_b15003e22 + urban_areas$cs_b15003e23 + urban_areas$cs_b15003e24 + urban_areas$cs_b15003e25) / urban_areas$cs_b15003e1

# Reading urban areas geography data
urban_areas_geo <- st_read(urban_areas_geo_path) %>% dplyr::rename(urb=UACE10)

urban_areas_geo$urb <- as.numeric(urban_areas_geo$urb)
urban_areas <- st_as_sf(urban_areas %>% left_join(urban_areas_geo, by='urb'))
urban_areas$Urban.Area  <- as.character(urban_areas$urb_name) %>% purrr::map(function (n) toString(
  unlist(strsplit(strsplit(n,", ")[[1]][[1]],"--")[[1]][[1]])
))

# Reading in simulated visits data 
iso <- read.csv(iso_data_path) 


## ==========================
## 3. Data Manipulation
## ==========================
# Join iso dataset with block groups data
iso <- iso %>% left_join(cbgs %>% dplyr::select(bg_geoid,urb_name) %>% dplyr::rename(bg_GEOID = bg_geoid), by='bg_GEOID')
iso <- iso %>% left_join(urban_areas %>% dplyr::select(urb_name,Urban.Area), by='urb_name')

walksheds <- slice_head(iso %>% filter(Urban.Area=='Chicago') %>% dplyr::select(Urban.Area,isochrone_geometry),n=1) %>% dplyr::rename(geometry = isochrone_geometry)
walksheds <- walksheds %>% mutate(geometry = st_transform(st_as_sfc(geometry,crs=4326),3857))
walksheds <- st_sf(walksheds, crs=3857)

walkshed <- walksheds$geometry
cx_w <- unname(st_coordinates(st_centroid(walkshed)))[1]
cy_w <- unname(st_coordinates(st_centroid(walkshed)))[2]


cbg_geo <- slice_head(iso %>% dplyr::filter(Urban.Area=='Chicago') %>% dplyr::select(Urban.Area,blockgroup_geometry),n=1) %>% dplyr::rename(geometry = blockgroup_geometry)
cbg_geo <- cbg_geo %>% mutate(geometry = st_transform(st_as_sfc(geometry,crs=4326),3857))
cbg_geo <- st_sf(cbg_geo, crs=3857)
cbg_center <- st_centroid(cbg_geo$geometry)
ws <- slice_head(walksheds,n=1)$geometry
pois_labels <- c("AC"="Arts & Culture",
                 "G"="Grocery",
                 "D"="Drugstore",
                 "H"="Health",
                 "P"="Park",
                 "REL"="Religious",
                 "RES"="Restaurant",
                 "SCH"="School",
                 "SER"="Services")

## ==========================
## 4. Setting Up Map Details
## ==========================

# Setting up API key for mapbox and base URL for map tiles
bbox <- bb(cx = cx_w, cy = cy_w + 1000, width=7500, height=7500, current.projection=3857)

# Set API key for Mapbox and base URL for map tiles.
apiKey <- paste0("?access_token=", "pk.eyJ1IjoidGFiYmlhc292IiwiYSI6ImNqYXJtaDZvcTRnb2UycXBkYzJ4OG42bmkifQ.ASyqoW1tK4cqSNsbyUIN7Q")
baseUrl <- "https://api.mapbox.com/styles/v1/tabbiasov/cl3ag5eyo003y14p0xmjxpglh/tiles/256/{z}/{x}/{y}"

# Fetch base map for the US.
basemap <- read_osm(bbox, type=paste0(baseUrl,apiKey))

## ==========================
## 5. Define Visualization Palettes
## ==========================

pal <- c("#81f495", "#475841", "#0b0033",
         "#d8abe7","#096f53","#f25d46","#e4ca23","#9a024e","#075db4")
pal2 <- c("#0a711b", "#475841", "#0b0033",
          "#d8abe7","#096f53","#f25d46","#e4ca23","#9a024e","#075db4")

pois_all <- iso %>% filter(Urban.Area=='Chicago') %>% dplyr::select(visits,cat15,Urban.Area,poi_geometry) %>% dplyr::rename(geometry = poi_geometry)
pois_all <- pois_all %>% mutate(geometry = st_transform(st_as_sfc(geometry,crs=4326),3857))
pois_all <- st_sf(pois_all, crs=3857)
pois_all$in_15m <- st_intersects(pois_all$geometry,ws,sparse = FALSE) 
pois_all$in_bbox <- st_intersects(pois_all$geometry,st_as_sfc(bbox),sparse = FALSE)

set.seed(921) 

pois <- filter(pois_all,in_bbox, visits>0) %>% sample_n(75)
lines <- mutate(pois, line = st_cast(st_union(geometry,cbg_center),"LINESTRING"))
lines <- st_sf(lines %>% st_drop_geometry() %>% dplyr::rename(geometry = line), crs=3857)
pois_grp <- pois %>% group_by(cat15,in_15m) %>% summarize(visits = sum(visits))


## ==========================
## 6. Generate Visualizations
## ==========================

# Generating the first map visualizing walksheds and points of interest
map_a <- tm_shape(basemap) + 
  tm_rgb() +
  tm_shape(walksheds) +
  tm_polygons("#64B3A3", lwd = 2.5, alpha = 0.1, lty = "dashed", border.col = '#64B3A3', border.alpha = 1) + 
  tm_shape(filter(pois, in_15m)) + 
  tm_dots(
    size = 0.15,
    col = 'cat15',
    palette = pal,
    title = 'Category',
    legend.show = FALSE,
    labels = pois_labels
  ) + 
  tm_shape(filter(pois, !in_15m)) + 
  tm_dots(
    col = 'cat15',
    palette = pal,
    alpha = 0.5,
    legend.show = FALSE,
    size = 0.15
  )

# Generating the second map visualizing walksheds, lines, and points of interest
map_u <- tm_shape(basemap) + 
  tm_rgb() +
  tm_shape(walksheds) +
  tm_polygons("#64B3A3", lwd = 2.5, alpha = 0.1, lty = "dashed", border.col = '#64B3A3', border.alpha = 1) + 
  tm_shape(filter(pois, in_15m)) + 
  tm_dots(
    size = 0.15,
    col = 'cat15',
    palette = pal,
    title = 'Category',
    legend.show = FALSE,
    labels = pois_labels
  ) + 
  tm_shape(filter(pois, !in_15m)) + 
  tm_dots(
    col = 'cat15',
    palette = pal,
    alpha = 0.5,
    legend.show = FALSE,
    size = 0.15
  ) + 
  tm_shape(filter(lines, in_15m)) + 
  tm_lines(col = 'cat15', palette = pal, legend.col.show = FALSE, lwd = 1.5) + 
  tm_shape(filter(st_difference(lines, walksheds$geometry), !in_15m, in_bbox)) + 
  tm_lines(col = 'cat15', palette = pal, legend.col.show = FALSE, alpha = 0.2, lwd = 1.5) +
  tm_shape(filter(st_intersection(lines, walksheds$geometry), !in_15m, in_bbox)) + 
  tm_lines(col = 'cat15', palette = pal, legend.col.show = FALSE, alpha = 0, lwd = 1.5)
                                                                             

# Legend for usage plot
usage_dash <- ggplot(data=pois_grp, aes(x=cat15, color=cat15, y=visits)) +
    geom_bar(aes(fill = cat15, alpha=in_15m),
             stat = 'identity',
             position="fill",
             size=0.3,
             show.legend=FALSE,
             width = 0.725,
            ) +
    scale_x_discrete(labels=pois_labels)+ 
    scale_fill_manual(
        limits = names(pois_labels),
        values = pal) +
    scale_color_manual(
        limits = names(pois_labels),
        values = pal2) +
    coord_flip() + 
    theme_classic() + 
    labs(
            subtitle = "% trips within 15m",
            x = element_blank(),
            y = element_blank(),
            # fill = "Within 15m:"
        ) + 
    theme(legend.position="top",
              axis.text.x.bottom = element_blank(),
              axis.ticks.x.bottom = element_blank(),
              axis.line.x.bottom = element_blank(),
              axis.text.y.right = element_blank(),
              axis.ticks.y.right = element_blank(),
              axis.line.y.right = element_blank(),
              legend.justification="left",
              legend.direction = "horizontal",
              legend.text=element_text(size=10, family="Helvetica"),
              axis.text.y = element_text(face=600, color="black",size=8, family="Helvetica"),
              plot.subtitle = element_text(hjust=2, vjust=-1.05,size=10, face=800, family="Helvetica"),
              axis.line = element_line(colour = 'black', size = 0.2),
              axis.ticks=element_line(size=0.2),
              plot.margin = margin(
                  t = 0, r = 0, b =-5, l =0, 
                  unit = "points"),
              legend.margin=margin(0,0,0,0),
              legend.key.size = unit(0.7,"line"),
              panel.background = element_rect(fill = "transparent", colour = NA),
              plot.background = element_rect(fill = "transparent", colour = NA)
             )

# Legend for access plot
access_dash <- ggplot(data=filter(pois, in_15m), aes(x=cat15)) +
    geom_bar(aes(fill = cat15),
             stat = 'count',
             show.legend=FALSE,
             width = 0.725,
            ) +
    scale_x_discrete(labels=pois_labels)+ 
    scale_fill_manual(
        limits = names(pois_labels),
        values = pal) +
    geom_text(aes(label = ..count..), color='black',
              size = 2.8, family="Helvetica",
              stat = "count",
              hjust = -0.5,
              position = position_dodge(width = 1), inherit.aes = TRUE
             ) + 
    coord_flip() + 
    theme_classic() + 
    labs(
            y = element_blank(),
            x = element_blank(),
            subtitle = "Access within 15m"
    ) + 
    ylim(0,20) + 
    theme(legend.position="top",
              axis.text.x.bottom = element_blank(),
              axis.ticks.x.bottom = element_blank(),
              axis.line.x.bottom = element_blank(),
              axis.text.y.right = element_blank(),
              axis.ticks.y.right = element_blank(),
              axis.line.y.right = element_blank(),
              legend.justification="left",
              legend.direction = "horizontal",
              legend.text=element_text(size=10),
              text=element_text(size=10,  family="Helvetica"),
              axis.text.y = element_text(face=600, color="black",size=8,family="Helvetica"),
              plot.subtitle = element_text(hjust=2, vjust=-1.05, face=800,size=10, family="Helvetica"),
              axis.line = element_line(colour = 'black', size = 0.2),
              axis.ticks=element_line(size=0.2),
              plot.margin = margin(
                  t = 0, r = 2, b =-5, l =0, 
                  unit = "points"),
              legend.margin=margin(0,0,0,0),
              legend.key.size = unit(0.7,"line"),
              panel.background = element_rect(fill = "transparent", colour = NA),
              plot.background = element_rect(fill = "transparent", colour = NA)
             )
                                                                             
## ==========================
## 7. Generate Composite Figure
## ==========================
output_figure_path <- file.path(main_path, "output", "figure_1.pdf")

pdf(file = output_figure_path, width = 9, height = 5)

top_vp <- viewport(
  x = 0, y = 0, 
  width = 1, height = 1,
  layout = grid.layout(nrow = 1, ncol = 2, widths = c(0.5, 0.5)),
  just = c("left", "bottom")
)
pushViewport(top_vp)
print(map_a, vp = viewport(layout.pos.col = 1, layout.pos.row = 1))
print(map_u, vp = viewport(layout.pos.col = 2, layout.pos.row = 1))

vp_l <- viewport(
  x = 0, y = 0, 
  width = 0.5, height = 1
)
pushViewport(vp_l)
vp_d1 <- viewport(
  x = 1, y = 0.680 - 0.0825 + 0.5 + 0.05, 
  width = 0.478, height = 0.38 - 0.05,
  just = c("left", "bottom")
)
pushViewport(vp_d1)
grid.draw(roundrectGrob(gp = gpar(alpha = 0.9, col = 'grey', lwd = 0), r = unit(0.00, "snpc")))
grid.draw(ggplotGrob(access_dash))
popViewport()
popViewport()
vp_r <- viewport(
  x = 0.5, y = 0, 
  width = 0.5, height = 1
)
pushViewport(vp_r)

vp_d2 <- viewport(
  x = 1, y = 0.680 - 0.0825 + 0.5 + 0.05, 
  width = 0.478, height = 0.38 - 0.05,
  just = c("left", "bottom")
)
pushViewport(vp_d2)
grid.draw(roundrectGrob(gp = gpar(alpha = 0.9, col = 'grey', lwd = 0), r = unit(0.00, "snpc")))
grid.draw(ggplotGrob(usage_dash))
popViewport()
popViewport()
popViewport()
grid.text("a", x = 0.03, y = .946, gp = gpar(fontfamily = "Helvetica", fontface = "bold"))
grid.text("b", x = 0.53, y = .946, gp = gpar(fontfamily = "Helvetica", fontface = "bold"))
dev.off()
