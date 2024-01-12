## INPUTS
## -----------
## zoning1961.gpkg

## OUTPUTS
## -----------
## FigureS1.pdf

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
packages <- c("lfe", "ggplot2", "tidyverse", "stargazer", "haven", "fixest", "tmap", 
              "sf", "rosm", "prettymapr", "ggpubr", "tmaptools", "grid", 
              "gridExtra", "osmdata")

# Apply the function to each package
invisible(sapply(packages, install_and_load))

## ==========================
## Set Main Path (User should modify this only)
## ==========================

# Set working directory to main path
main_path <- "path/to/replication"  # User should replace this with the path to the 'replication' folder

# Define paths to data files
zoning_data_path <- file.path(main_path, "data", "clean", "zoning1961.gpkg")

# Reading the zoning shapefile
zoning1961 <- read_sf(zoning_data_path)


## ==========================
## 2. Figure
## ==========================


# Additional settings (bounding box for mapping, api key and url)
bbox_nyc = bb(c(c(-74.25909, 40.477399), c(-73.7001809, 40.9161785)), current.projection=4326, projection=2263)

# Set API key for Mapbox and base URL for map tiles.
apiKey <- paste0("?access_token=", "pk.eyJ1IjoidGFiYmlhc292IiwiYSI6ImNqYXJtaDZvcTRnb2UycXBkYzJ4OG42bmkifQ.ASyqoW1tK4cqSNsbyUIN7Q")
baseUrl <- "https://api.mapbox.com/styles/v1/tabbiasov/cl3ag5eyo003y14p0xmjxpglh/tiles/256/{z}/{x}/{y}"
bbox_us = bb(c(c(-124.848974, 24.396308),c(-66.885444, 49.384358)), current.projection = 4326, projection = 3857)

# Fetch base map for the US.
basemap_nyc <- read_osm(bbox_nyc, type=paste0(baseUrl,apiKey))


pdf(file =  "output/figureS1.pdf", width = 5, height = 5)
tm_shape(basemap_nyc) + 
  tm_rgb() +
  tm_shape(zoning1961 %>% filter(COMM == TRUE)) +
  tm_fill("FAR", palette=c("#f25d46","#e4ca23","#096f53"),
          legend.is.portrait=TRUE,
          breaks=c(0, 0.5, 1.0, 2.0, 3.4, 4.0, 5.0, 6.0, 10.0, 15.0),
          labels=as.character(c(0.5, 1.0, 2.0, 3.4, 4.0, 5.0, 6.0, 10.0, 15.0)),
          interval.closure='right'
  ) +
  tm_layout(fontfamily='Helvetica',
            legend.show = TRUE,
            main.title = "1961 NYC Zoning Resolution (Commercial Districts)",
            main.title.size = 1,
            title.position = c('center', 'top'), 
  )
dev.off()