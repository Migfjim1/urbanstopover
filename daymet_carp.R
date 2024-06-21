setwd("~/Documents/prjct_georgia_on_my_mind/")

rm(list = ls())
sp_data_dir = "../georgia_data/"
source("aux_func.R")

# Format bounding box to DayMet expected pattern (first upper left coordinates, 
# then lower right coordinates):
state_bbox_aea_daymet = c(state_bbox_aea[["ymax"]], 
                          state_bbox_aea[["xmin"]], 
                          state_bbox_aea[["ymin"]], 
                          state_bbox_aea[["xmax"]])

#------------------------------------------------------------------------------#
# Download and process DayMet data -----
#------------------------------------------------------------------------------#

## Data download (no need to run again) ----------------------------------------
# 
download_daymet_ncss(location = state_bbox_aea,
                     path = paste0(sp_data_dir, "daymet/raw/"),
                     start = 2000,
                     end = 2021,
                     frequency = "daily",
                     param = "tmin")

download_daymet_ncss(location = state_bbox_aea,
                     path = paste0(sp_data_dir, "daymet/raw/"),
                     start = 2000,
                     end = 2021,
                     frequency = "daily",
                     param = "tmax")

download_daymet_ncss(location = state_bbox_aea,
                     path = paste0(sp_data_dir, "daymet/raw/"),
                     start = 2000,
                     end = 2021,
                     frequency = "daily",
                     param = "prcp")

test = rast(paste0(sp_data_dir, "daymet/raw/prcp_daily_2000_ncss.nc"))

plot(test)
            
## Create seasonal rasters per year --------------------------------------------

create_daymet_mean_rasters = function(daymet_type, final_res, sp_data_dir){
  
  # Set up start and end days for spring and fall:
  spring_start = "14-03-"
  spring_end = "16-06-"
  
  fall_start = "14-08-"
  fall_end = "16-11-"
  
  tic()
  
  if (daymet_type == "tmax"){ # Monthly maximum temperature
    
    nc_files <- list.files(paste0(sp_data_dir, "daymet/raw/"),
                           pattern = glob2rx("*tmax*"),
                           full.names = TRUE)
    
  } else if(daymet_type == "tmin"){ # Monthly minimum temperature
    
    nc_files <- list.files(paste0(sp_data_dir, "daymet/raw/"),
                           pattern = glob2rx("*tmin*"),
                           full.names = TRUE)
    
  } else if(daymet_type == "prcp"){ # Precipitation
    
    nc_files <- list.files(paste0(sp_data_dir, "daymet/raw/"),
                           pattern = glob2rx("*prcp*"),
                           full.names = TRUE)
    
  } 
  
  fall_raster_list = list()
  spring_raster_list = list()
  
  count = 1
  for(nc_file in nc_files){
    
    nc_stack = rast(nc_file)
    
    curr_year = str_match(nc_file, "[0-9]{4}")
    
    print(paste0("Creating mean ", daymet_type, " raster for the spring of ", curr_year, "."))
    
    # Extract spring start and end dates for current year:
    curr_start = paste0(daymet_type, "_", yday(paste0(spring_start, curr_year)))
    curr_end = paste0(daymet_type, "_", yday(paste0(spring_end, curr_year)))
    
    # Get index of start and end dates in current raster stack:
    curr_start = which(names(nc_stack) == curr_start)
    curr_end = which(names(nc_stack) == curr_end)
    
    # Get mean raster for spring season:
    spring = mean(nc_stack[[curr_start:curr_end]], na.rm = TRUE)
    spring = project(spring, crs("EPSG:5070"))
    
    # Aggregate to match target resolution:
    fact = floor(final_res/1032.8)
    if(fact > 1){
      spring = aggregate(spring, fact = fact, fun = mean, na.rm = TRUE)
    }  
    
    dir.create(paste0(sp_data_dir, "daymet/processed/res", final_res, "/spring/"), showWarnings = FALSE, recursive = TRUE)
    writeRaster(spring, paste0(sp_data_dir, "daymet/processed/res", final_res, "/spring/", daymet_type, "_", curr_year, "_spring.tif"), overwrite = TRUE)
    
    print(paste0("Creating mean ", daymet_type, " raster for the fall of ", curr_year, "."))
    
    # Extract fall start and end dates for current year:
    curr_start = paste0(daymet_type, "_", yday(paste0(fall_start, curr_year)))
    curr_end = paste0(daymet_type, "_", yday(paste0(fall_end, curr_year)))
    
    # Get index of start and end dates in current raster stack:    
    curr_start = which(names(nc_stack) == curr_start)
    curr_end = which(names(nc_stack) == curr_end)
    
    # Get mean raster for fall season:
    fall = mean(nc_stack[[curr_start:curr_end]], na.rm = TRUE)
    fall = project(fall, crs("EPSG:5070"))
    
    # Aggregate to match target resolution:
    fact = floor(final_res/1032.8)
    if(fact > 1){
      fall = aggregate(fall, fact = fact, fun = mean, na.rm = TRUE)
    }
    
    dir.create(paste0(sp_data_dir, "daymet/processed/res", final_res, "/fall/"), showWarnings = FALSE, recursive = TRUE)
    writeRaster(fall, paste0(sp_data_dir, "daymet/processed/res", final_res, "/fall/", daymet_type, "_", curr_year, "_fall.tif"), overwrite = TRUE)
    
    count = count + 1
  }
  
  toc()
}

create_daymet_mean_rasters(daymet_type = "prcp", 1000, sp_data_dir)
create_daymet_mean_rasters(daymet_type = "tmin", 1000, sp_data_dir)
create_daymet_mean_rasters(daymet_type = "tmax", 1000, sp_data_dir)


