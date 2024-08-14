# set directory
data_dir = "/Users/aeroeco_green/Library/Mobile Documents/com~apple~CloudDocs/Desktop/csu/phd/projects/urban stopover/urbanstopover_git/urbanstopover/data"

#------------------------------------------------------------------------------#
# Load cbsa boundaries --------------------------------------------------------
#------------------------------------------------------------------------------#

# Load cbsa boundaries:
cbsa_bound = st_read(paste0(data_dir, "/cbsa_shps/top50_cbsa.shp"))

# for now - as I'm testing the scripts - I am subsetting to one cbsa so things run faster
# chose nashville bc it has highest coverage %
cbsa_bound <- cbsa_bound %>% 
  filter(CBSAFP == '34980')

cbsa_bound_aea = st_transform(cbsa_bound, crs("EPSG:5070"))

# Create a buffer around cbsa:
cbsa_buffer_aea = st_buffer(cbsa_bound, dist = 100000)
cbsa_bbox_aea = st_bbox(cbsa_buffer_aea)

#------------------------------------------------------------------------------#
# Data Carpentry ---------------------------------------------------------------
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
## Create template raster ------------------------------------------------------
#------------------------------------------------------------------------------#
# This function is used by the carpentry scripts to create a template file 
# at the target resolution. This template is used for resampling and aggregating
# the other rasters.
#------------------------------------------------------------------------------#

create_template = function(final_res, cbsa_bbox_aea, sp_data_dir){
  
  # Create template raster:
  template = rast(extent = cbsa_bbox_aea, 
                      crs = crs("EPSG:5070"), 
                      res = final_res)
  template[] = 1
  
  writeRaster(template, paste0(sp_data_dir, "template_res", final_res,".tif"), overwrite = TRUE)

  return(template)
}

#------------------------------------------------------------------------------#
## Create landscape metrics raster list ----------------------------------------
#------------------------------------------------------------------------------#
# This function will create a list of raster stacks for the desired landscape 
# metric at the target resolution. Each item in the list will be a raster stack 
# of all years of data for a given class. The stacks are in EPSG:5070 and match 
# the target resolution's template raster. 
# 
# data_type is one of: "lsm_c_ed", "lsm_c_pd", "lsm_c_pland"
# list names are: DATA_TYPE + _ + LANDCOVER_CLASS
# raster stack names are: DATA_TYPE + _ + YEAR
#------------------------------------------------------------------------------#

create_lsm_raster_list = function(data_type, final_res){
  
  tif_files <- list.files(paste0(sp_data_dir, "modis/processed/res", final_res, "/", data_type),
                          full.names = TRUE)
  
  # Create hierarchical raster list where each item is a raster list containing 
  # all years of rasters of a given class:
  raster_list = list()
  for(tif_file in tif_files){
    
    curr_year = str_sub(str_match(tif_file, "[0-9]{4}_"), 1, -2)
    curr_class = paste0(data_type, "_", str_sub(str_match(tif_file, "[0-9]+.tif"), 1, -5))
    
    raster_list[[curr_class]] = c(raster_list[[curr_class]], rast(tif_file))
    
    # If it`s the first element of the list, then index it explicitly, else, 
    # access the last element of the list for the current class:
    if(length(raster_list[[curr_class]]) == 1){
      names(raster_list[[curr_class]])[1] = paste0(curr_class, "_", curr_year)
    } else {
      names(raster_list[[curr_class]])[length(names(raster_list[[curr_class]]))] = paste0(curr_class, "_", curr_year)
    }
  }
  
  # Load raster template for resampling:
  template = rast(paste0(sp_data_dir, "template_res", final_res, ".tif"))
  
  # Resample all rasters in each list:
  for(count in seq(1, length(raster_list))){
    
    raster_list[[count]] = rast(raster_list[[count]])
    
    # Resample to match template:
    raster_list[[count]] = terra::resample(raster_list[[count]], template, method = "bilinear")
    
  } 
  
  return(raster_list)
}

#------------------------------------------------------------------------------#
## Create DayMet raster list ---------------------------------------------------
#------------------------------------------------------------------------------#
# This function will create a list of raster stacks for the desired season 
# at the target resolution. Each item in the list will be a raster stack of all
# years of data for a given data type (tmin, tmax, prcp). The stacks are in 
# EPSG:5070 and match the target resolution's template raster. 
# 
# season is one of: "spring", "fall"
# list names are: "tmin", "tmax", "prcp" (data types)
# raster stack names are: DATA_TYPE + _ + YEAR + "_" + SEASON
#------------------------------------------------------------------------------#

create_daymet_raster_list = function(season, final_res, sp_data_dir){

  processed_path = paste0(sp_data_dir, "daymet/processed/res", final_res, "/")

  all_tif_files <- c(tmin = list(list.files(paste0(processed_path, season),
                                            pattern = glob2rx("tmin*"),
                                            full.names = TRUE)), 
                     tmax = list(list.files(paste0(processed_path, season),
                                            pattern = glob2rx("tmax*"),
                                            full.names = TRUE)),
                     prcp = list(list.files(paste0(processed_path, season),
                                            pattern = glob2rx("prcp*"),
                                            full.names = TRUE)))
  raster_list = list()
  classes = c("tmin", "tmax", "prcp")
  
  for(curr_class in classes){
    
    cat(paste("Starting class:", curr_class, "\n"))
    
    # Select all tif_files of current class:
    tif_files = all_tif_files[[curr_class]]

    # Create a raster stack of all years of data of current class:
    raster_list[[curr_class]] = rast(tif_files)
    
    # Rename current raster stack: 
    names(raster_list[[curr_class]]) = str_match(sources(raster_list[[curr_class]]), paste0(curr_class, "_[0-9]{4}_", season))[,1]
  }
  
  # Load raster template for resampling:
  template = rast(paste0(sp_data_dir, "template_res", final_res, ".tif"))
  
  for(count in seq(1, length(raster_list))){
    # Resample to match template:
    raster_list[[count]] = terra::resample(raster_list[[count]], template, method = "bilinear")
  }
  
  return(raster_list)
}

#------------------------------------------------------------------------------#
## Reproject, resample, and apply raster mask ----------------------------------
#------------------------------------------------------------------------------#
# This function will reproject and resample a given clutter raster_mask and 
# apply it to a year_rast (use case is a raster of data for one station-year).
#
# Typically, we would also mask beam blockage pixels. Georgia stations, however, 
# had no beam blockage (checked on June 29th). 
#------------------------------------------------------------------------------#

station_mask = function(raster_mask, year_rast){

  # Reproject it: 
  raster_mask = project(raster_mask, "EPSG:5070", method = "mode")

  # Resample it to match current raster:
  raster_mask = terra::resample(raster_mask, year_rast, method = "near")
  
  # Mask current raster:
  year_rast = mask(year_rast, raster_mask, maskvalues = 1)
  
  return(year_rast)
}

#------------------------------------------------------------------------------#
## Create radar raster list ----------------------------------------------------
#------------------------------------------------------------------------------#
# This function will create a raster stack for the desired season at the target 
# resolution. Each raster in the stack is a mosaic of the rasters of each 
# station for a given year. Only season-station-years with more than 75% of 95
# days sampled are added to the stacks. The stacks are in EPSG:5070 and match 
# the target resolution's template raster. 
# 
# season is one of: "spring", "fall"
# raster stack names are: "radar" + SEASON + _ + YEAR
#------------------------------------------------------------------------------#

create_radar_raster_list = function(season, final_res){
  
  folders <- list.files(paste0(sp_data_dir, "radar_data/processed/res", final_res, "/", season),
                          full.names = TRUE)
  
  data_comp = read.csv(paste0(sp_data_dir, "radar_data/data_completeness.csv"))
  
  # Load template geometry:
  template = rast(paste0(sp_data_dir, "template_res", final_res, ".tif"))
  
  raster_list = list()
  count = 1
  for(folder in folders){
    
    radar_files = list.files(folder, full.names = TRUE)
    
    curr_year = str_sub(str_match(folder, "/[0-9]{4}"), 2, -1)
    
    # Create empty list of files for current year:
    year_list = list()
    
    for(radar_file in radar_files){
      
      curr_station = str_match(radar_file, "[A-Z]{4}")[1]
      
      sampling_count = data_comp[((data_comp$year == curr_year) & (data_comp$station == curr_station)), paste0("count_", season)]
      
      # If more than 75% of season was sampled, add it to the mosaic:
      if(sampling_count > 71){
        
        year_rast = rast(radar_file)
      
        # Load and apply clutter mask for current station:
        raster_mask = rast(paste0(sp_data_dir,"radar_data/clutter/", curr_station, ".tif"))
        year_rast = station_mask(raster_mask, year_rast)
        
        year_list = c(year_list, year_rast)  
      }
    }
    
    year_list = sprc(year_list)
    
    # Mosaic station ranges using the max function to capture peak activity:
    year_mosaic = mosaic(year_list, fun = max)
    
    # Resample mosaic to match template:
    year_mosaic = terra::resample(year_mosaic, template)
    
    # Extend mosaic to template extent:
    year_mosaic = extend(year_mosaic, template)
    
    # Append to raster_list:
    raster_list = c(raster_list, year_mosaic)
    names(raster_list)[count] = paste0("radar_", season, "_", curr_year)
    count = count + 1
  }
  
  return(rast(raster_list))
}

#------------------------------------------------------------------------------#
## Tabularize Daymet raster list for a given season ----------------------------
#------------------------------------------------------------------------------#

tabularize_daymet = function(season, final_res, backbone_df, sp_data_dir){
  
  print(paste0("Tabularizing DayMet data for the ", season, "."))
  
  temp = create_daymet_raster_list(season, final_res, sp_data_dir)
  
  main_df = data.table()
  
  count = 1
  for(i in 1:length(temp)){
    
    # Convert raster layer to dataframe:
    temp_df = as.data.table(temp[[i]], xy = TRUE)
    
    # Merge to backbone with pixel ids:
    temp_df = merge(backbone_df, temp_df, all.x = TRUE)
    
    # Melt all columns with data per year into a single column:
    temp_df = melt(temp_df, id.vars = c(1, 2, 3))
    
    # Name the data column with the label of current class:
    names(temp_df)[names(temp_df)=="value"] = names(temp)[i]
    
    # Extract year from variable name:
    temp_df$year = str_match(temp_df$variable, "[0-9]{4}")[,1]
    
    # Remove unnecessary column:
    temp_df$variable = NULL
    
    if(count == 1){
      main_df = temp_df
    } else {
      main_df = merge(main_df, temp_df, all.x = TRUE, by = c("x", "y", "pixel_id", "year"))
    }
    count = count + 1
  } # end loop through raster stack
  
  file_path = paste0(sp_data_dir, "tabular_data/res", final_res, "/")
  dir.create(file_path, recursive = TRUE)
  fwrite(main_df, paste0(file_path, "processed_daymet_", season, ".csv"))
  
  create_hist_panels(vars_of_int = c("tmin", "tmax", "prcp"), main_df, season)
}

#------------------------------------------------------------------------------#
## Tabularize radar raster list for a given season -----------------------------
#------------------------------------------------------------------------------#

tabularize_radar = function(season, final_res, backbone_df, sp_data_dir){
  
  print(paste0("Tabularizing radar data for the ", season, "."))
  
  main_df = create_radar_raster_list(season, final_res)
  
  # Convert raster stack to dataframe:
  main_df = as.data.table(main_df, xy = TRUE)
  
  # Merge to backbone with pixel ids:
  main_df = merge(backbone_df, main_df, all.x = TRUE)
  
  # Melt all columns with data per year into a single column:
  main_df = melt(main_df, id.vars = c(1, 2, 3))
  
  # Name the data column with the label of current class:
  names(main_df)[names(main_df)=="value"] = "radar_response"
  
  # Extract year from variable name:
  main_df$year = str_match(main_df$variable, "[0-9]{4}")[,1]
  
  # Remove unnecessary column:
  main_df$variable = NULL
  
  file_path = paste0(sp_data_dir, "tabular_data/res", final_res, "/")
  dir.create(file_path, recursive = TRUE)
  fwrite(main_df, paste0(file_path, "processed_radar_", season, ".csv"))
  
  create_hist_panels(vars_of_int = c("radar_response"), main_df, season)
}

#------------------------------------------------------------------------------#
## Create final datasets for given season and approach -------------------------
#------------------------------------------------------------------------------#

create_final_dataset = function(season, backbone_peryear_df, final_res, approach){
  
  print(paste0("Creating final dataset for lsm approach in the ", season, "."))
  
  season_files = list.files(paste0(sp_data_dir, "tabular_data/res", final_res),
                            pattern = glob2rx(paste0("processed*", season, "*")), 
                            full.names = TRUE)
  
  # Append DayMet data to backbone:
  df = append_tabular_data(backbone_peryear_df, season_files[1])
  
  # Append radar response:
  df = append_tabular_data(df, season_files[2])
  
  # Append landcover data according to approach:
  if(approach == "lsm"){
    
    # Append lsm data:
    lsm_file = paste0(sp_data_dir, "tabular_data/res", final_res, "/processed_lsm.csv")
    df = append_tabular_data(df, lsm_file)
    
  } else if(approach == "modis"){
    
    # Append modis data:
    modis_file = paste0(sp_data_dir, "tabular_data/res", final_res, "/processed_modis.csv")
    df = append_tabular_data(df, modis_file)
    
  }
  
  # Append distance to radar:
  temp = read.csv("../data_georgia/radar_data/dist_to_station.csv")
  
  # Fix some issues that cause the merging to fail: 
  temp$x = round(temp$x, 2)
  temp$y = round(temp$y, 2)
  
  df = merge(df, temp, all.x = TRUE, by = c("x", "y", "pixel_id"))
  df$X = NULL
  df$site = NULL
  
  fwrite(df, paste0(sp_data_dir, "tabular_data/res", final_res, "/final_dataset_", approach, "_", season, ".csv"))
  
}

#------------------------------------------------------------------------------#
## Create histograms of variables of interest per year of a given dataframe ----
#------------------------------------------------------------------------------#

create_hist_panels = function(vars_of_int, df, season){
  
  for(var_of_int in vars_of_int){
    
    # Plot histograms of response variable on each year:
    p = df %>% 
      ggplot()+
      geom_histogram(aes(x = .data[[var_of_int]]))+
      facet_wrap(~year)
    
    # Save as png:
    ggsave(filename = paste0("../data_georgia/tabular_data/res", final_res, "/hists/hists_", var_of_int, "_", season, ".png"), 
           plot =p, dpi = 600, width = 20, height = 15, units = "cm")
    
  }
}

#------------------------------------------------------------------------------#
## Append tabular data to current dataframe (merge) ----------------------------
#------------------------------------------------------------------------------#

append_tabular_data = function(df, tabular_file){
  
  temp = fread(tabular_file)

  # Fix some issues that cause the merging to fail: 
  temp$x = round(temp$x, 2)
  temp$y = round(temp$y, 2)

  df = merge(df, temp, all.x = TRUE, by = c("x", "y", "pixel_id", "year"))

  return(df)
}

#------------------------------------------------------------------------------#
## Create an sf object of everything but ocean in our study region -----------
#------------------------------------------------------------------------------#

create_not_ocean = function(){
  
  # Load and filter shapefile of North America:
  shape <- read_sf(dsn = "../phd_data/spatial_data/NorthAmerica_political_boundaries/Political_Boundaries_(Area)-shp/77653ced-5baf-493d-a76a-e5fab228a5152020328-1-ag2xcn.e0tm.shp")
  shape = shape %>% 
    filter(COUNTRY == "USA") %>%
    filter(!NAME %in% c("Alaska", "Hawaii", "Guam", 
                        "Puerto Rico", "United States Virgin Islands", 
                        "Navassa Island"))
  
  # Get the union of all state polygons (the buffering removes
  # some space between Georgia and Florida, and making it 
  # valid fixes issues with the polygons):
  shape = st_buffer(shape, dist = 10)
  shape = st_union(shape, by_feature = TRUE, is_coverage = TRUE)
  shape = st_union(shape)
  shape = st_make_valid(shape)
  
  # Create sf object with outer boundary of study region:
  temp = create_template(final_res, cbsa_bbox_aea, sp_data_dir)
  temp_sf = st_as_sf(as.polygons(temp))
  temp_sf = st_transform(temp_sf, st_crs(shape))
  
  # Crop the United States outline to our study region:
  shape = st_crop(shape, temp_sf)
  
  shape = st_transform(shape, "EPSG:5070")

  return(shape)
}

#------------------------------------------------------------------------------#
## Create predictions map ------------------------------------------------------
#------------------------------------------------------------------------------#

create_map = function(df, xgboost_model, approach, season, col_range, pred_year, model_run){
  
  if(approach == "lsm"){
    if(season == "spring"){
      map_title = paste("LSM Approach: Predictions for Spring of", pred_year)
    } else if(season == "fall"){
      map_title = paste("LSM Approach: Predictions for Fall of", pred_year)
    }
  } else if(approach == "modis"){
    if(season == "spring"){
      map_title = paste("MODIS Approach: Predictions for Spring of", pred_year)
    } else if(season == "fall"){
      map_title = paste("MODIS Approach: Predictions for Fall of", pred_year)
    }
  } 
  
  pred_df = as.data.frame(df[df$year == pred_year, ] %>% 
    arrange(pixel_id) %>%
    relocate(radar_response))
  
  pred_df$dist_to_station = 30
  
  # Predict onto the entire 2020 dataset:
  pred_df = pred_df[,c("x", "y", "pixel_id", xgboost_model$feature_names)]
  pred_df$predictions = predict(xgboost_model, as.matrix(pred_df[, col_range]))

  p = ggplot()+
    geom_raster(data=pred_df, aes(x=x, y=y, fill=predictions))+
    geom_sf(data = cbsa_bound_aea, fill = NA, linewidth = 1, color = "black")+
    theme_classic()+
    scale_fill_viridis_c()+
    labs(y="Latitude", x="Longitude", fill="Stopover Density", 
         title = map_title, 
         subtitle = "Georgia outline in black")+
    theme(text=element_text(size=10))
  p
  
  ggsave(filename = paste0("./model_outputs/figures/", model_run, "_", approach, "_", season, "_model_predictions.png"),
         plot = p, dpi=600, width = 20, height = 15, units = "cm")
  
}

#------------------------------------------------------------------------------#
# Predictions -----
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## Predict migration on a given year, save it as a raster ----------------------
#------------------------------------------------------------------------------#

predict_migration = function(curr_year, df, season, model_run, approach, final_res){
  
  tic()
  cat(paste("Starting year: ", curr_year, "\n"))
  
  pred_df = as.data.frame(df[df$year == curr_year, ] %>% 
                            arrange(pixel_id) %>%
                            relocate(radar_response))
  
  pred_df$dist_to_station = 30
  
  # Load model:
  xgboost_model = xgb.load(paste0("./model_outputs/", model_run, "_", approach, "_", season, "_xgboost.json"))
  xgboost_model <- xgb.Booster.complete(xgboost_model)
  
  # Predict on current year:
  pred_df$predictions = predict(xgboost_model, as.matrix(pred_df[, 5:ncol(pred_df)]))
  
  # Create a template raster to rasterize dataframe:
  temp = create_template(final_res, cbsa_bbox_aea, sp_data_dir)
  pred = setValues(temp, pred_df$predictions)
  
  # Create a mask to set ocean pixels do NA:
  not_ocean_sf = create_not_ocean()
  
  # Mask the ocean out:
  pred = mask(x = pred, mask = vect(not_ocean_sf), touches = TRUE)
  
  # Format layer name:
  names(pred) = paste0(season, "_", curr_year)
  
  writeRaster(pred, paste0("./model_outputs/raster_predictions/", season, "/", curr_year, ".tiff"), overwrite = TRUE)
  
  toc()
}

#------------------------------------------------------------------------------#
## Create raster stack of season predictions clipped to a given region ---------
#------------------------------------------------------------------------------#

create_pred_raster_list = function(season, sub_shape){
  
  # Create a list of all rasters for a given season:
  raster_list = list.files(paste0("./model_outputs/raster_predictions/", season, "/"), full.names = TRUE)
  raster_list = rast(raster_list)
  
  raster_list = mask(raster_list, sub_shape)
  
  return(raster_list)
  
}

#------------------------------------------------------------------------------#
## Create high intensity raster for given season -------------------------------
#------------------------------------------------------------------------------#

create_high_intensity_raster = function(raster_list, season, subset_name){
  
  # Load empty template raster, set all cells to 0:
  high_intensity_raster = create_template(final_res, cbsa_bbox_aea, sp_data_dir)
  high_intensity_raster = high_intensity_raster - 1
  
  for(i in 1:dim(raster_list)[3]){
    
    curr_raster = raster_list[[i]]
    
    # Set to 1 cells at the upper 10% of activity for current year, and to 0 
    # cell below that threshold:
    q90 = quantile(values(curr_raster), 0.9, na.rm = TRUE)
    curr_raster[curr_raster < q90] = 0
    curr_raster[curr_raster >= q90] = 1
    
    # Create raster with sum of all years when each cell was at the upper 10% 
    # of activity:
    high_intensity_raster = high_intensity_raster + curr_raster
  }
  
  # Trim the final raster to the political_sf polygon, to make
  # the borders of Georgia in the ocean match the political sf:
  shape = create_not_ocean()
  high_intensity_raster = mask(high_intensity_raster, vect(shape))
  
  writeRaster(high_intensity_raster, paste0("./model_outputs/raster_predictions/", season, "_", subset_name, "_high_intensity.gpkg"), 
              overwrite = TRUE)
  #writeRaster(high_intensity_raster, paste0("./model_outputs/raster_predictions/", season, "_", subset_name, "_high_intensity.tiff"), overwrite = TRUE)

}

#------------------------------------------------------------------------------#
## Create hotspot raster for given season -------------------------------
#------------------------------------------------------------------------------#

create_hotspot_raster = function(season, subset_type){
  
  # Load raster with number of high intensity years:
  high_intensity_raster = rast(paste0("./model_outputs/raster_predictions/", season, "_", subset_type, "_high_intensity.tiff"))
  
  # Create a hotspot raster with pixels that had more than 15 
  # high-intensity years:
  hotspot_raster = high_intensity_raster
  
  hotspot_raster[high_intensity_raster >= 10] = 1
  hotspot_raster[high_intensity_raster < 10] = 0
  
  # Trim the final raster to the political_sf polygon, to make
  # the borders of Georgia in the ocean match the political sf:
  shape = create_not_ocean()
  hotspot_raster = mask(hotspot_raster, vect(shape))
  
  writeRaster(hotspot_raster, paste0("./model_outputs/raster_predictions/", season, "_", subset_type, "_hotspots.gpkg"), 
              overwrite = TRUE)
}

#------------------------------------------------------------------------------#
## Create Low-Med-High raster ------------------
#------------------------------------------------------------------------------#

low_med_high = function(raster_obj){
  
  q33 = quantile(values(raster_obj), 0.33, na.rm = TRUE)
  q66 = quantile(values(raster_obj), 0.66, na.rm = TRUE)

  # Create bins of relative variation:
  new_raster = create_template(final_res, cbsa_bbox_aea, sp_data_dir)

  new_raster[raster_obj > q66] = 3
  new_raster[(raster_obj > q33) & (raster_obj < q66)] = 2
  new_raster[(raster_obj < q33)] = 1
  
  return(new_raster)
}

#------------------------------------------------------------------------------#
## Create raster of coefficient of variation for given season ------------------
#------------------------------------------------------------------------------#

create_cv_raster = function(raster_list, season, subset_type){

  coeff_var = function(x){
    return(sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))
  }
  
  temp = app(raster_list, fun = coeff_var)
  
  # Create low, med, high bins of variation:
  cv_raster = low_med_high(temp)
  
  # Mask to the state of Georgia:
  cv_raster = mask(cv_raster, cbsa_bound_aea)
  
  # Trim the final raster to the political_sf polygon, to make
  # the borders of Georgia in the ocean match the political sf:
  shape = create_not_ocean()
  cv_raster = mask(cv_raster, vect(shape))
  
  writeRaster(cv_raster, paste0("./model_outputs/raster_predictions/", season, "_", subset_type, "_coeff_of_var.gpkg"), 
              overwrite = TRUE)
  
}

#------------------------------------------------------------------------------#
# Map Plots -----
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## Build base plot of Georgia State --------------------------------------------
#------------------------------------------------------------------------------#

plot_base_Georgia = function(){
  
  # Create some variables to plot the maps:
  map_extent = ext(cbsa_bound_aea)
  
  political_sf = st_read("../phd_data/spatial_data/NorthAmerica_political_boundaries/Political_Boundaries_(Area)-shp/77653ced-5baf-493d-a76a-e5fab228a5152020328-1-ag2xcn.e0tm.shp")
  political_sf = st_transform(political_sf, "EPSG:5070")
  
  xmin = map_extent[1]
  xmax = map_extent[2]
  ymin = map_extent[3]
  ymax = map_extent[4]
  
  p = ggplot()+
    geom_sf(data = political_sf, fill = NA, color = "black", linewidth = 0.5)+
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))+
    theme_classic()+
    theme(axis.line=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          plot.background=element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(), 
          axis.title = element_blank(),
          text = element_text(family = "montserrat"))+
    
    # spatial-aware automagic scale bar
    annotation_scale(location = "tr", height=unit(0.2, 'cm'), pad_y = unit(2, "cm"), 
                     text_family = "montserrat", style = "ticks")+
    
    annotation_north_arrow(location = "tr", height=unit(0.7, 'cm'),
                           width = unit(0.7, "cm"),
                           which_north = "true",
                           pad_x = unit(0.5, "cm"), pad_y = unit(2.5, "cm"),
                           style = north_arrow_orienteering(text_size = 8,
                                                            text_family = "montserrat"))
  return(p)

}


#------------------------------------------------------------------------------#
## Plot high intensity raster for given season -------------------------------
#------------------------------------------------------------------------------#

plot_high_intensity_raster = function(season, subset_name){
  
  high_intensity_raster = rast(paste0("./model_outputs/raster_predictions/", season, "_", subset_name, "_high_intensity.tiff"))
  
  high_intensity_df = as.data.frame(high_intensity_raster, xy = TRUE)
  
  # Create base plot:
  base_plot = plot_base_Georgia()
  
  # Create new data layer:
  new_layers = geom_raster(data = high_intensity_df, aes(x = x, y = y, fill = lyr.1))

  # Append data layer to the first position in the base plot:
  base_plot$layers = append(base_plot$layers, new_layers, 0)
  
  p = base_plot +
    scale_fill_gradient(high = "#DF2927", low = "#ffffff")+
    labs(title = paste0(tools::toTitleCase(season), ": Number of Years of High\nIntensity Migration Stopover"),
         fill = "Number\nof Years", 
         family = "montserrat")
  p
  
  # Save as pdf:
  ggsave(filename = paste0("./model_outputs/final_maps/", season, "_", subset_name, "_high_intensity.pdf"), 
         plot =p, bg = "white", width = 16, height = 15, units = "cm")
  
  # Save as png:
  ggsave(filename = paste0("./model_outputs/final_maps/", season, "_", subset_name, "_high_intensity.png"), 
         plot = p, bg = "white", dpi = 600, width = 16, height = 15, units = "cm")
  
}


#------------------------------------------------------------------------------#
## Plot hotspot raster for given season ----------------------------------------
#------------------------------------------------------------------------------#

plot_hotspot_raster = function(season, subset_type){
  
  hotspot_raster = rast(paste0("./model_outputs/raster_predictions/", season, "_", subset_type, "_hotspots.tiff"))
  hotspot_df = as.data.frame(hotspot_raster, xy = TRUE)
  
  hotspot_df$lyr.1 = ifelse(hotspot_df$lyr.1 == 0, NA, 1)
  
  # Create base plot:
  base_plot = plot_base_Georgia()
  
  # Create new data layer:
  new_layers = geom_raster(data = hotspot_df, aes(x = x, y = y, fill = as.factor(lyr.1)))
    
  # Append data layer to the first position in the base plot:
  base_plot$layers = append(base_plot$layers, new_layers, 0)
  
  p = base_plot +
    scale_fill_manual(breaks = 1, values = "#DF2927", na.value="white")+
    labs(title = paste0(tools::toTitleCase(season), ": Hotspots of Migration Stopover"))+
    guides(fill = "none")
  
  p
  
  # Save as pdf:
  ggsave(filename = paste0("./model_outputs/final_maps/", season, "_", subset_type, "_hotspots.pdf"), 
         plot =p, bg = "white", width = 14, height = 15, units = "cm")
  
  # Save as png:
  ggsave(filename = paste0("./model_outputs/final_maps/", season, "_", subset_type, "_hotspots.png"), 
         plot =p, bg = "white", dpi = 600, width = 14, height = 15, units = "cm")
}


#------------------------------------------------------------------------------#
## Plot raster of coefficient of variation for given season ------------------
#------------------------------------------------------------------------------#

plot_cv_raster = function(season, subset_type){
  
  cv_raster = rast(paste0("./model_outputs/raster_predictions/", season, "_", subset_type, "_coeff_of_var.tiff"))
  
  cv_df = as.data.frame(cv_raster, xy = TRUE)
  
  # Create base plot:
  base_plot = plot_base_Georgia()
  
  # Create new data layer:
  new_layers =  geom_raster(data = cv_df, aes(x = x, y = y, fill = as.factor(lyr.1)))
    
  # Append data layer to the first position in the base plot:
  base_plot$layers = append(base_plot$layers, new_layers, 0)
  
  # Configure the aesthetics:
  p = base_plot + 
    scale_fill_manual(values = c("#dffffe", "#8EE0DE", "#00a19b"), 
                      labels = c("Low", "Medium", "High"), na.value="white")+
    labs(title = paste0(tools::toTitleCase(season), ": Inter-Year Variation in Migration Intensity"),
         fill = "Coefficient\nof Variation")

  p
  
  # Save as pdf:
  ggsave(filename = paste0("./model_outputs/final_maps/", season, "_", subset_type, "_coeff_of_var.pdf"), 
         plot =p, width = 17, height = 15, units = "cm")
  
  # Save as png:
  ggsave(filename = paste0("./model_outputs/final_maps/", season, "_", subset_type, "_coeff_of_var.png"), 
         plot =p, bg = "white", dpi = 600, width = 17, height = 15, units = "cm")

}


#------------------------------------------------------------------------------#
## Plot raster of average intensity for given season ------------------
#------------------------------------------------------------------------------#

plot_average_raster = function(season){
  
  # Create a list of all rasters for a given season:
  raster_list = list.files(paste0("./model_outputs/raster_predictions/", season, "/"), full.names = TRUE)
  raster_list = rast(raster_list)
  
  average_raster = mean(raster_list)
  average_raster = low_med_high(average_raster)
 
  # Trim the final raster to the political_sf polygon, to make
  # the borders of Georgia in the ocean match the political sf:
  shape = create_not_ocean()
  average_raster = mask(average_raster, vect(shape))
  
  writeRaster(average_raster, paste0("./model_outputs/raster_predictions/", season, "_average.tiff"), overwrite = TRUE)
  
  average_df = as.data.frame(average_raster, xy = TRUE)
  
  # Create some variables to plot the maps:
  map_extent = ext(cbsa_bound_aea)
  
  political_sf = st_read("../phd_data/spatial_data/NorthAmerica_political_boundaries/Political_Boundaries_(Area)-shp/77653ced-5baf-493d-a76a-e5fab228a5152020328-1-ag2xcn.e0tm.shp")
  political_sf = st_transform(political_sf, "EPSG:5070")
  
  xmin = map_extent[1]
  xmax = map_extent[2]
  ymin = map_extent[3]
  ymax = map_extent[4]
  
  p = ggplot()+
    geom_raster(data = average_df, aes(x = x, y = y, fill = as.factor(lyr.1)))+
    geom_sf(data = political_sf, fill = NA, color = "black", linewidth = 0.5)+
    scale_fill_manual(values = c("#ffffff", "#FFCE33", "#DF2927"), 
                    labels = c("Low", "Medium", "High"), na.value="white")+
    labs(title = paste0(tools::toTitleCase(season), ": Average Migration Stopover Intensity"),
         subtitle = "Average taken across all years from 2001 to 2020.",
         fill = "Migration Stopover\nIntensity")+
    geom_sf(data = rivers_sf, fill = NA, color = "black", linewidth = 0.5, linetype = "longdash")+
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))+
    theme_classic()+
    theme(axis.line=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          plot.background=element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(), 
          axis.title = element_blank(),
          text = element_text(family = "montserrat"))+
    
    # spatial-aware automagic scale bar
    annotation_scale(location = "br", height=unit(0.2, 'cm'), 
                     #pad_y = unit(2, "cm"), 
                     text_family = "montserrat", style = "bar")+
    
    annotation_north_arrow(location = "br", height=unit(0.5, 'cm'),
                           width = unit(0.5, "cm"),
                           which_north = "true",
                           pad_x = unit(0.1, "cm"), pad_y = unit(0.7, "cm"),
                           style = north_arrow_orienteering(text_size = 8,
                                                            text_family = "montserrat"))+
  geom_label_repel(data = rivers_sf, aes(label = river_name, x = X+pad_x, y = Y+pad_y), 
            size = 3, family = "montserrat", seed = 42)
  # Configure the aesthetics:
  p
  
  # Save as pdf:
  ggsave(filename = paste0("./model_outputs/final_maps/", season, "_average.pdf"), 
         plot =p, width = 17, height = 15, units = "cm")
  
  # Save as png:
  ggsave(filename = paste0("./model_outputs/final_maps/", season, "_average.png"), 
         plot =p, bg = "white", dpi = 600, width = 17, height = 15, units = "cm")
  
}

#------------------------------------------------------------------------------#
## Plot Okefenokkee Maps -------------------------------------------
#------------------------------------------------------------------------------#

plot_okefenokee = function(season, subset_type, google_sat){
  
  # Load hotspot raster and make it into polygons:
  hotspot_raster = rast(paste0("./model_outputs/raster_predictions/", season, "_", subset_type, "_hotspots.geotiff"))
  hotspot_raster = project(hotspot_raster, "EPSG:4326")
  
  hotspot_sf = st_as_sf(as.polygons(hotspot_raster))
  hotspot_sf = st_cast(hotspot_sf, "POLYGON") %>%
    filter(lyr.1 != 0)
  
  hotspot_df = as.data.frame(hotspot_raster, xy = TRUE)
  
  hotspot_df$lyr.1 = ifelse(hotspot_df$lyr.1 == 0, NA, 1)
  
  # Get extent of the main plot:
  rect_lon = c(-82.8,-82)
  rect_lat = c(30.45, 31.1)
  
  outer = matrix(c(min(rect_lon), min(rect_lat), 
                   max(rect_lon), min(rect_lat),
                   max(rect_lon), max(rect_lat),
                   min(rect_lon), max(rect_lat),
                   min(rect_lon), min(rect_lat)),
                 ncol = 2, byrow = TRUE)
  
  
  rect_sf = st_polygon(list(outer)) %>%
    st_sfc() %>%
    st_as_sf()
  
  st_crs(rect_sf) = "EPSG:4326"
  
  p = ggmap(google_sat)+
    geom_sf(data = political_sf, fill = NA, inherit.aes = FALSE, 
            color = "black", linewidth = 0.7, linetype = "dashed")+
    geom_sf(data = nwr_oke, aes(fill = "nwr"), color = "white", linetype = "dashed", alpha = 0.2, inherit.aes = FALSE, 
            color = "black")+
    geom_sf(data = hotspot_sf, aes(fill = "hotspots"), alpha = 0.5, inherit.aes = FALSE, 
            color = "black", linewidth = 0.5)+
    geom_sf(data = twin_pines, aes(fill = "twinpines"), alpha = 0.6, inherit.aes = FALSE, 
            color = "black", linewidth = 0.5)+
    geom_sf(data = nwr_oke, fill = NA, color = "white", inherit.aes = FALSE, 
            linetype = "dashed")+
    geom_label(x = -82.75, y = 30.47, 
               hjust = "left",
               family = "merriweather", size = 3.2,
               fill = "gray89", alpha = 0.4, label.r = unit(0, "cm"),
               label = "Source: USGS, Google, USFWS\nTwin Pines Minerals, LLC")+
    scale_fill_manual(name = "", guide = 'legend',
                      breaks = c("nwr", "hotspots", "twinpines"),
                      labels = c('Okefenokee National Wildlife Refuge', 
                                 "Stopover Hotspots",
                                 "Proposed Twin Pines Minerals, LCC\nmining region"),
                      values = c("gray85", "#DF2927", "goldenrod1"), na.value = NA) +
    coord_sf(ylim = c(min(rect_lat), max(rect_lat)), xlim = c(min(rect_lon), max(rect_lon)))+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(), 
          title = element_text(size = 20, family = "merriweather", vjust = 10),
          legend.spacing.y = unit(0, "mm"), 
          legend.key.size = unit(0.8, "cm"),
          legend.text = element_text(size = 11, family = "merriweather"),
          legend.position = c(0.217, 0.13),
          legend.background = element_blank(),
          legend.margin = margin(-0.3,0.4,0.3,0.3,unit = "cm"),
          legend.box.background = element_rect(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA),
          plot.margin=unit(c(1,1,1,1), "cm"))+
    labs(x = "", y = "",
         title = paste0(tools::toTitleCase(season), ": Stopover Hotspots for Bird Migration"))+
    # spatial-aware automagic scale bar
    annotation_scale(location = "bl", 
                     pad_y = unit(0.7, "cm"),
                     pad_x = unit(10, "cm"),
                     height = unit(0.5, 'cm'),
                     style = "bar",
                     text_col = "white")+
    annotation_north_arrow(which_north = "true", 
                           style = north_arrow_fancy_orienteering(line_col = "white", text_col = "white"),  
                           location = "bl",
                           pad_y = unit(1.4, "cm"),
                           pad_x = unit(10.5, "cm"))
  
  
  
  # Create some variables to plot the maps:
  map_extent = ext(st_transform(cbsa_bound_aea, "EPSG:4326"))
  
  xmin = map_extent[1]
  xmax = map_extent[2]
  ymin = map_extent[3]
  ymax = map_extent[4]
  
  p_inset = ggplot()+
    geom_sf(data = political_sf, fill = NA, inherit.aes = FALSE, 
            color = "black", linewidth = 0.5, linetype = "dashed")+
    geom_sf(data = hotspot_sf, aes(fill = "hotspots"), inherit.aes = FALSE, 
            color = "black", linewidth = 0.04)+
    geom_sf(data = rect_sf, fill = NA, color = "black", linewidth = 0.8, inherit.aes = FALSE)+
    scale_fill_manual(name = "", guide = 'legend',
                      breaks = c("hotspots"),
                      values = c("#DF2927"), na.value = NA) +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))+
    theme_classic()+
    theme(axis.line=element_blank(),
          panel.border=element_rect(fill = NA, color = "black", linewidth = 3),
          axis.text = element_blank(),
          axis.ticks = element_blank(), 
          axis.title = element_blank(),
          plot.margin=unit(c(0,0,-0.2,-0.2), "cm"),
          text = element_text(family = "merriweather"))+
    guides(fill = "none")
  
  
  p_full = ggdraw(p)+
    draw_plot(p_inset,
              # The distance along a (0,1) x-axis to draw the left edge of the plot
              x = 0.075, 
              # The distance along a (0,1) y-axis to draw the bottom edge of the plot
              y = 0.55,
              # The width and height of the plot expressed as proportion of the entire ggdraw object
              width = 0.35, 
              height = 0.35)
  
  # Save as pdf:
  ggsave(filename = paste0("./model_outputs/final_maps/", season, "_twinpines_hotspots.pdf"), 
         plot = p_full, bg = "white", width = 25, height = 25, units = "cm")
  
}
