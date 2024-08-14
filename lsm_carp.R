rm(list = ls())
source("setup.R")
source("aux_func.R")

#------------------------------------------------------------------------------#
# Download NLCD data (no need to rerun) ----------------------------------------
#------------------------------------------------------------------------------#

nlcd_years = c(2001, 2004, 2006, 2008, 2011, 2016, 2019)

for(curr_year in nlcd_years){
  
  tic()
  
  get_nlcd(template = cbsa_buffer_aea,
           year = curr_year,
           dataset = "landcover",
           extraction.dir = paste0(data_dir, "nlcd/landcover/"),
           label = paste0("LC", "_", curr_year))
  
  get_nlcd(template = cbsa_buffer_aea,
           year = curr_year,
           dataset = "impervious",
           extraction.dir = paste0(data_dir, "nlcd/impervious/"),
           label = paste0("imp", "_", curr_year))

  toc()
}

#------------------------------------------------------------------------------#
# Create rasters of landscape metrics ------------------------------------------
#------------------------------------------------------------------------------#

get_metrics = function(point_dist, buffer_size, 
                       data_source, 
                       lsm_metric){
  
  # This function will generate rasters with calculated landscape metric for each
  # class in input landcover raster, for each year of data available.
  
  # - lsm_metric: class-level metric to calculate
  # - data_source: either "modis" or "nlcd"
  # - buffer_size: half-size of each pixel in final raster
  # - point_dist: distance between pixel midpoints in final raster

  cat(paste("Running metric", lsm_metric, "at", point_dist, "m resolution.\n"))
  
  landcover_files =  list.files(paste0(data_dir, data_source, "/raw"),
                                pattern = glob2rx("*.tif"),
                                full.names = TRUE)
  
  # Register the parallel backend to run each file in one thread:
  cl = makeCluster(8)
  registerDoParallel(cl)
  env_packages = c("terra", "tictoc", "landscapemetrics", "dplyr", "stringr",
                   "sf")

  # Create final directory for files of current resolution and metric:
  file_path = paste0(data_dir, data_source, "/processed/res", point_dist, "/", lsm_metric , "/")
  dir.create(file_path, recursive = TRUE)
  
  foreach(landcover_file = landcover_files, 
          .export = ls(globalenv()), 
          .packages = env_packages,
          .verbose = TRUE) %dopar% {
            
    # Load template at final resolution (point_dist) to get extent:
    temp = create_template(point_dist, cbsa_bbox_aea, data_dir)
            
    # Create a grid of x and y points within temp raster extent:
    xrange = seq(xmin(temp), xmax(temp), point_dist)
    yrange = seq(ymin(temp), ymax(temp), point_dist)
    point_grid = as.matrix(expand.grid(xrange, yrange))
            
    # Save the grid in a dataframe and create plot_id column for later:
    point_grid_df = as.data.frame(point_grid)
    point_grid_df$plot_id = seq(1, nrow(point_grid_df), 1)
    
    cat(paste(landcover_file, "\n"))
    tic()
    
    landcover_raster = rast(landcover_file)
    landcover_raster = project(landcover_raster, crs("EPSG:5070"), method = "near")
    landcover_raster = crop(landcover_raster, cbsa_buffer_aea)
    landcover_raster[landcover_raster == 0] = NA
    landcover_raster[is.na(landcover_raster)] = NA
    
    if(data_source == "nlcd"){
      landcover_raster = as.numeric(landcover_raster)
      landcover_raster = aggregate(landcover_raster, fact = 16, method = "near")
    }

    curr_year = str_match(landcover_file, "[0-9]{4}")
    
    lsm_df = sample_lsm(
      landcover_raster,
      point_grid,
      plot_id = NULL,
      shape = "square",
      size = buffer_size,
      all_classes = TRUE,
      return_raster = FALSE,
      what = lsm_metric,
      verbose = TRUE,
      progress = TRUE)
    
    for(landcover_class in unique(lsm_df$class, na.rm = TRUE)){
      
      temp = lsm_df %>%
        filter(class == landcover_class)
      
      if(nrow(temp != 0)){
        # Merge current class results to the point_grid_df by plot_id:
        temp = merge(temp, point_grid_df, by = "plot_id")
        
        # Create a raster:
        temp = rast(temp[,c("Var1", "Var2", "value")], type = "xyz")
        crs(temp) = crs(landcover_raster)
        
        temp = project(temp, crs("EPSG:5070"))
      
        writeRaster(temp, paste0(file_path, data_source, "_", curr_year, "_", landcover_class, ".tif"), overwrite = TRUE)
        
      }
    }
    toc()
  }
}

#------------------------------------------------------------------------------#
## Percentage of each class (PLAND) ---------------------------------------------
#------------------------------------------------------------------------------#
#
# Calculate the percentage of each class within a square of 3km size.
# See: https://r-spatialecology.github.io/landscapemetrics/reference/lsm_c_pland.html?q=pland#ref-usage
# 

get_metrics(point_dist = 1000, 
            buffer_size = 3000,
            lsm_metric = "lsm_c_pland", data_source = "modis")

#------------------------------------------------------------------------------#
## Patch density (PD) ----------------------------------------------------------
#------------------------------------------------------------------------------#
#
# Calculate number of patches of each class, divided by landscape area
# See: https://r-spatialecology.github.io/landscapemetrics/reference/lsm_c_pd.html
# 

get_metrics(point_dist = 1000, 
            buffer_size = 3000,
            lsm_metric = "lsm_c_pd", data_source = "modis")

#------------------------------------------------------------------------------#
## Edge density (ED) -----------------------------------------------------------
#------------------------------------------------------------------------------#
#
# Calculate length of the edge of each class divided by landscape area
# See: https://r-spatialecology.github.io/landscapemetrics/reference/lsm_c_ed.html
# 

get_metrics(point_dist = 1000, 
            buffer_size = 3000,
            lsm_metric = "lsm_c_ed", data_source = "modis")


#------------------------------------------------------------------------------#
## Largest Patch Index (LPI) -----------------------------------------------------------
#------------------------------------------------------------------------------#
#
# Calculate the area of the largest patch of given class, divided by the total
# landscape area. 
# See: https://r-spatialecology.github.io/landscapemetrics/reference/lsm_c_lpi.html?q=lpi
#

get_metrics(point_dist = 1000, 
            buffer_size = 3000,
            lsm_metric = "lsm_c_lpi", data_source = "modis")

