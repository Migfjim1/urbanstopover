source("setup.R")

# Read in the shapefile
counties_shp <- st_read("data/cb_2018_us_county_20m/cb_2018_us_county_20m.shp")

# Combine the STATEFP and COUNTYFP fields and convert it to an integer
counties_shp$STATE_COUNTY_FP <- paste0(counties_shp$STATEFP, counties_shp$COUNTYFP)
counties_shp$STATE_COUNTY_FP <- as.integer(counties_shp$STATE_COUNTY_FP)

# read in NCHS codes
counties_nchs <- read.csv("data/NCHSURCodes2013.csv")

counties_shp$STATE_COUNTY_FP <- as.integer(counties_shp$STATE_COUNTY_FP)

# merge the nchs fields to the county shapefile attribute table
counties_shp <- counties_shp %>%
  left_join(counties_nchs, by = "STATE_COUNTY_FP")

# View the updated attribute table
head(counties_shp)

# remove alaska, hawaii, and puerto rico
counties_shp <- counties_shp %>%
  filter(!(STATEFP %in% c("02", "15", "72")))

# Create the ggplot
ggplot(data = counties_shp) +
  geom_sf(aes(fill = X2013.code)) +               # Map counties and color by X2013.code
  scale_fill_viridis_c(option = "viridis") +      # Use the viridis color scheme
  theme_minimal() +                               # Apply a minimal theme
  theme(
    panel.grid = element_blank(),                 # Remove grid lines
    axis.title.x = element_blank(),               # Remove x-axis label
    axis.title.y = element_blank(),               # Remove y-axis label
    legend.title = element_blank(),               # Remove legend title
    plot.title = element_blank()                  # Remove plot title
  )

# Optionally, save the merged shapefile
st_write(counties_shp, "data/processed/counties.shp", append=TRUE)
