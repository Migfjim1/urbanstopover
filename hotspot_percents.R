# source setip script to install/load necessary packages
source("/Users/mjimenez/Downloads/urbanstopover-main/setup.R") # may need to change file path to where setup.R is stored

# define filepath to data download - see Github README for Dryad DOI
data_fold<-("/Users/mjimenez/Downloads/doi_10_5061_dryad_1jwstqk68__v20250908")

### percent of stopover hotspots in metro areas
# read in spring and fall hotspots
hotspot_sp <- rast(file.path(data_fold, "spring_stopover_2500_v9_265_class.tif"))
hotspot_fa <- rast(file.path(data_fold, "fall_stopover_2500_v9_265_class.tif"))

# Read in counties shapefile
counties_shp <- st_read(file.path(data_fold, "counties.shp/counties.shp"))
cbsa_shp <- st_read(file.path(data_fold, "tl_2019_us_cbsa.shp/tl_2019_us_cbsa.shp"))
urbcore_shp <- st_read(file.path(data_fold, "tl_2023_us_uac20.shp/tl_2023_us_uac20.shp"))

# remove non-continental states
counties_shp <- counties_shp[!counties_shp$STATEFP %in% c("02", "72", "15"), ]

# set crs to EPSG:5070
proj_crs <- "EPSG:5070"
counties_shp <- st_transform(counties_shp, proj_crs)
urbcore_shp <- st_transform(urbcore_shp, proj_crs)
cbsa_shp <- st_transform(cbsa_shp, proj_crs)


## create the stopover hotspot layers
# write a function that takes hotspot raster, converts to sf polygon
process_hotspot <- function(raster_layer) {
  raster_layer[raster_layer != 2] <- NA # this is subsetting to the 90th percentile layer
  st_as_sf(as.polygons(raster_layer)) # converting it to a sf multipolygon
}
# run it for fa + sp
hotspot_fa_sf <- process_hotspot(`hotspot_fa`$fall_stopover_2500_v9)
hotspot_sp_sf <- process_hotspot(`hotspot_sp`$spring_stopover_2500_v9)

hotspot_fa_sf <- st_transform(hotspot_fa_sf, proj_crs)
hotspot_sp_sf <- st_transform(hotspot_sp_sf, proj_crs)
# just a little viz check to make sure this looks right
#plot(hotspot_sp_sf, col = "red", border = NA)


## create different urban boundary layer (i.e. different definitions of "urban")
# using CDC definitions
metro_counties <- counties_shp[counties_shp$X2013_c > 0 & counties_shp$X2013_c < 5, ] 
lgmetro_counties <- counties_shp[counties_shp$X2013_c > 0 & counties_shp$X2013_c < 3, ]

# subset combined stats areas to metropolitan stats area
msa_shp <- cbsa_shp[cbsa_shp$LSAD == "M1", ]

# need to convert each type into multipolygons for sum stats
# organized these in least to most restrictive definition
metro_polys <- st_union(metro_counties) # CDC 'metropolitan areas'
msa_polys <- st_union(msa_shp) # US Census 'combined statistical areas
lgmetro_polys <- st_union(lgmetro_counties) # CDC 'large metropolitan areas'
core_polys <- st_union(urbcore_shp) # US Census 'Urban Areas'

# create a list of these different urban definitions 
urban_definitions <- list(
  "msa_area" = msa_polys,
  "metro_area" = metro_polys,
  "lgmetro_area" = lgmetro_polys,
  "core_area" = core_polys
)

# store the total area of the continental U.S.
counties_polys <- st_union(counties_shp) 
total_area <- sum(st_area(counties_shp))

# function to calculate percentages for each urban definition
calculate_percentages <- function(urban_layer, hotspot_fa, hotspot_sp) {
  # get the land area as a percentage of the continental u.s.
  urban_area <- sum(st_area(urban_layer))
  land_area_pct <- (urban_area / total_area) * 100
  
  # calculate the percentage of fall hotspots in the urban area
  intersection_fa <- st_intersection(hotspot_fa, urban_layer)
  area_intersection_fa <- sum(st_area(intersection_fa))
  area_hotspot_fa <- sum(st_area(hotspot_fa))
  hotspot_pct_fa <- (area_intersection_fa / area_hotspot_fa) * 100
  
  # calculate the percentage of spring hotspots in the urban area
  intersection_sp <- st_intersection(hotspot_sp, urban_layer)
  area_intersection_sp <- sum(st_area(intersection_sp))
  area_hotspot_sp <- sum(st_area(hotspot_sp))
  hotspot_pct_sp <- (area_intersection_sp / area_hotspot_sp) * 100
  
  # return the percentages as a named vector
  return(c(land_area_pct, hotspot_pct_fa, hotspot_pct_sp))
}

# run function with urban definitions to calculate percentages
percents_df <- do.call(rbind, lapply(names(urban_definitions), function(definition) {
  urban_layer <- urban_definitions[[definition]]
  percentages <- calculate_percentages(urban_layer, hotspot_fa_sf, hotspot_sp_sf)
  data.frame(
    definition = definition,
    land_area = as.numeric(percentages[1]),
    hotspot_percentage_fall = as.numeric(percentages[2]),
    hotspot_percentage_spring = as.numeric(percentages[3]),
    stringsAsFactors = FALSE
  )
}))

# Ensure the output is a data frame
percents_df <- as.data.frame(percents_df)

# calculate ratios
percents_df$ratio_fa <- percents_df$hotspot_percentage_fall/percents_df$land_area
percents_df$ratio_sp <- percents_df$hotspot_percentage_spring/percents_df$land_area
percents_df


