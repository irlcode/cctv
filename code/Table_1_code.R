########################################################
" Serebrennikov D., Skougarevskiy D. Tales of 4 cities "
"                  Code. Table 1                       "
"                Summary statistics                    "
########################################################

# This code separately calculates the different summary statistics for Table 1.



##################
## Preparations ## -------------------------------------
##################  


options(scipen = 999)

# Install the required packages
pkgs <- c("sf", "dplyr", "data.table", "mapview", "geojsonsf", "geojsonio", "geojson", "lubridate", "units", "stringi", "mapview", "stringr", "jsonlite", "jsonify", 'httr')

if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkgs, rownames(installed.packages())))
}

# Load packages
lapply(pkgs, library, character.only = T)


# Function
transform_spat <- function(x){
  {
    if (country == 'RU') { projection_used <- 32637 }
    if (country == 'SC') { projection_used <- 32630 }
    if (country == 'KZ') { projection_used <- 32642 }
    if (country == 'BE') { projection_used <- 31370 }
    if (country == 'FR') { projection_used <- 27561 }
  }
  
  x <- st_as_sf(x)
  x <- st_transform(x, crs = projection_used)
}

##################
## Calculations ## -------------------------------------
##################  


# Number of amenities

results <- data.frame()

for (i in c("RU", "SC", 'BE', 'FR')) {

   without_cctv <- fread(paste0("D:/serebrennikov/serebrennikov_cctv_project/v2. with different cities/data/prepared_data/", i, "_all_osm_objects_1_wide.csv"))
   total <- fread(paste0("D:/serebrennikov/serebrennikov_cctv_project/v2. with different cities/outputs/k-NN/with_pop/", i, "_buffer_wide_100.csv"))


  results <- rbind(results,
                   c(i, nrow(without_cctv), nrow(total)))           
  }
colnames(results) <- c("country", 'without_cctv', "total")
results

# ------------------------------------------------------------------------------


# Number of CCTV
results <- data.frame()

for (i in c("RU", "SC", 'BE', 'FR')) {
  
  load(paste0('D:/serebrennikov/serebrennikov_cctv_project/v2. with different cities/data/world_pop/', i, "_pop_grid_500.rdata"))
  
  # Load changed cctv datasets. 
  load(paste0("D:/serebrennikov/serebrennikov_cctv_project/v2. with different cities/data/prepared_data/", i, "_cctv.rdata"))
  
  spatial_objects <- c("pop_city_500", "cctv")
  
  for(v in spatial_objects) {
    assign(v, transform_spat(get(v)))
  }
  
  results <- rbind(results,
                   c(i, nrow(cctv)))           
}
colnames(results) <- c("country", "cctv_total")
results

# ------------------------------------------------------------------------------


# Population
for (i in c("RU", 'FR', 'BE', "SC")) {
  load(paste0("D:/serebrennikov/serebrennikov_cctv_project/v2. with different cities/data/world_pop/", i, "_pop_grid_500.Rdata"))
  print(paste0(i, " - ", round(sum(pop_city_500$population))))
}

# ------------------------------------------------------------------------------


# N of city objects
for (country in c('RU', 'FR', 'BE', 'SC')) {
  temp <- readRDS(paste0("D:/serebrennikov/serebrennikov_cctv_project/v2. with different cities/data/prepared_data/", country, "_all_objects_in_city_WIDE.rds"))
  
  print(nrow(temp))
}

# ------------------------------------------------------------------------------


# Amenities types (incl. frequent)
for (i in c("RU", 'FR', 'BE', "SC")) {
    working_df_fin <- fread(file = paste0("D:/serebrennikov/serebrennikov_cctv_project/v2. with different cities/data/prepared_data/", i, "_all_osm_objects_1_wide.csv")) 
    df <- fread(paste0("D:/serebrennikov/serebrennikov_cctv_project/v2. with different cities/outputs/k-NN/with_pop/", i, "_buffer_wide_100.csv"))
    setDT(df)
    df <- df[,-c('yes', 'distance', 'population')]
    if (country == "SC" | country == "FR") {
      df <- df[,-'surveillance']
    }
    df <- df[,-c("cctv_OSM_ID", "local_id", "cctv_ID", "OSM_ID", "NAME", "NAME_EN", "NAME_RU", "geom_type", "OSM_TYPE")]
    if (country != "FR") {
      df[, parking := parking + surface]
      df[, parking := ifelse(parking>0,1,0)] 
      df[, surface := NULL]  }

    print(paste0(
      i,
      ' --- Amenity types: ',
      ncol(working_df_fin)-1,
      ' --- Frequent amenity types: ',
      ncol(df)
    ))
}
 
# ------------------------------------------------------------------------------


# Percent for some OSM categories

# Function for it
load_and_calc <- function(tag) {
  
    results <- data.frame()
    for (i in c("RU", 'FR', 'BE', "SC")) {
      tryCatch({

      working_df_fin <- readRDS(file = paste0("D:/serebrennikov/serebrennikov_cctv_project/v2. with different cities/data/prepared_data/", i, "_all_objects_in_city_WIDE.rds")) 
      if (i == "q") {
        working_df_fin[, parking := parking_space + parking_entrance]
        working_df_fin[, parking := ifelse(parking>0,1,0)] 
        working_df_fin[, c('parking_entrance','parking_space') := NULL] 
      }
      if (i == "q"| i == 'q') {
        working_df_fin[, parking := surface + parking_space + parking_entrance]
        working_df_fin[, parking := ifelse(parking>0,1,0)] 
        working_df_fin[, c('surface', 'parking_entrance','parking_space') := NULL] 
      }
      if (i == 'q') {
        working_df_fin[, parking := surface + parking_entrance]
        working_df_fin[, parking := ifelse(parking>0,1,0)] 
        working_df_fin[, c('surface', 'parking_entrance') := NULL] 
        }
      
      temp <- round(100*prop.table(table(
        ifelse(working_df_fin[[tag]]>0,1,0)
      )),2)
    
      results <- rbind(results,
                       c(i, temp))
    },
    error=function(e) {
      results <- rbind(results,
                       c(i, c(NA,NA)))})
    }
    colnames(results) <- c("country", "0", "1")
    return(results)
    
}

#Calculate:

# Apartments
load_and_calc("apartments")

# school
load_and_calc("school")

# bus_stop
load_and_calc("bus_stop")

# bar_or_pub
load_and_calc("bar_or_pub")

# playground
load_and_calc("playground")

# pharmacy
load_and_calc("pharmacy")

# park
load_and_calc("park")

# parking
load_and_calc("parking")

# place_of_worship
load_and_calc("fast_food")

# bank
load_and_calc("bank")

# ------------------------------------------------------------------------------


for (country in c("RU", 'FR', 'BE', "SC")) {
  
  # Make column which will be use in analysis
  freqtab <- readRDS(paste0("D:/serebrennikov/serebrennikov_cctv_project/v2. with different cities/data/prepared_data/", country, "_all_osm_objects_Freqtab.RDS"))
  setDT(freqtab)
  freqtab[,
          tag_name := gsub('shop_shop_', 'shop_', tag_name)]
  
  col_interest <- c(freqtab[tag_full>49]$tag_name)
  col_interest <- col_interest[col_interest %ni% c('bench','waste_basket','waste_disposal','vending_machine', 'post_box', 'yes', 'pole', "garden", "bar", 'pub', 'garage', 'shop_yes')]
  col_interest <- col_interest[col_interest %ni% freqtab[grepl('shop', tag_name)][tag_full<100]$tag_name]
  
  # load full data
  all_objects <- fread(paste0("D:/serebrennikov/serebrennikov_cctv_project/v2. with different cities/outputs/k-NN/with_pop/", country, "_buffer_wide_", cutoff, "_RandSamp.csv"), encoding = 'UTF-8')
  
  # Make y column
  all_objects[,
              y := ifelse(!is.na(cctv_ID),1,0)]
  
  # Prepare data
  df <- all_objects[, .SD,
                    .SDcols = c('common_ID', 'y', 'distance', 'population', 'bar_or_pub',col_interest)]
  df[,3:ncol(df)] <- lapply(df[,3:ncol(df)], as.numeric)
  
  df <- df[y==1]

  df_central <- round(100*prop.table(table(
    ifelse(df$distance<3000,1,0)
  )),1)
  
  # Good column names for df
  colnames(df) <- make.names(colnames(df))
  df <- df[,-1][,-1][,-1][,-1]
  
  rs <- round(100*prop.table(table(
    ifelse(rowSums(df)>0,1,0)
  )),1)
  
  rm <- mean(rowMeans(df))
  
  print(country); print(rs); print(df_central); print(rm)

}

# ------------------------------------------------------------------------------


# Calculations of some spatial values

# Set parameters
country <- "FR" # choose RU (Moscow) / FR (Paris) / BE (Brussels) / SC (Edinburge)
cutoff <- 100 # Standard
units(cutoff) <- with(ud_units, m)

# ------------------------------------------------------------------------------


# District boundaries data load
if (country == 'RU') {
  district_boundaries <- st_read("boundary-polygon-lvl8.shp", stringsAsFactors = F)
  # Remove "New Moscow" because its territory joined to Moscow less than 10 years ago and its spatial morphology differs from 'Old' Moscow
  district_boundaries <- transform_spat(as.data.table(district_boundaries)[!ADMIN_L5 %in% c('Троицкий административный округ', 'Новомосковский административный округ')])
  
}
if (country == 'SC') {
  district_boundaries <- st_read("boundary-polygon-lvl6.shp", stringsAsFactors = F)
  district_boundaries <- district_boundaries %>% filter(NAME == "City of Edinburgh")
}
if (country == 'BE') {
  district_boundaries <- st_read("boundary-polygon-lvl4.shp", stringsAsFactors = F)
}
if (country == 'FR') {
  district_boundaries <- st_read("boundary-polygon-lvl6.shp", stringsAsFactors = F)
  district_boundaries <- district_boundaries %>% 
    filter(NAME == "Paris")
}

# CCTV data load
load(paste0('D:/serebrennikov/serebrennikov_cctv_project/v2. with different cities/data/prepared_data/', country, "_cctv.rdata"))

# Prepare
cctv_union <- st_union(st_buffer(cctv,cutoff))
cctv_union <- transform_spat(cctv_union)
district_boundaries <- transform_spat(district_boundaries)

# City area
round(st_area(district_boundaries),-6)/1000000

# ------------------------------------------------------------------------------


# Amenities per km^2
total <- nrow(fread(paste0("D:/serebrennikov/serebrennikov_cctv_project/v2. with different cities/data/prepared_data/", country, "_all_osm_objects_1_wide.csv")))
(total/(round(st_area(district_boundaries),-6)/1000000))

# ------------------------------------------------------------------------------


# CCTV area
round(st_area(cctv_union),-6)

# CCTV area percent
st_area(cctv_union)/st_area(district_boundaries)*100

# ------------------------------------------------------------------------------


# Mean CCTV distance to city center, number and percent of CCTV in city center
for (country in c("RU", 'FR', 'BE', "SC")) {
  
  df <- fread(paste0("data/", country, "_buffer_wide_", cutoff, ".csv"))
  cctv_df <- df[!is.na(cctv_ID),]
  df_non_cctv <- df[is.na(cctv_ID),]
  
  print(country)
  ten_percent_distance <- nrow(cctv_df[distance <= max(cctv_df$distance)/100*10])
  
  # Distance
  print(paste0(round(mean(cctv_df$distance),3), ' - se - ', se(cctv_df$distance)))
  print(paste0(ten_percent_distance, " - % - ", round(ten_percent_distance/nrow(cctv_df)*100,3)))

  cctv_df <- cctv_df[,-c("cctv_OSM_ID", "local_id", "cctv_ID", "OSM_ID", "NAME", "NAME_EN", "NAME_RU", "geom_type", "OSM_TYPE", 'distance', 'population')]
  df_non_cctv <- df_non_cctv[,-c("cctv_OSM_ID", "local_id", "cctv_ID", "OSM_ID", "NAME", "NAME_EN", "NAME_RU", "geom_type", "OSM_TYPE", 'distance', 'population')]

  # Number and percent of CCTV in city center
  print(paste0(round(mean(rowSums(df_non_cctv)),3), " - se - ", round(se(rowSums(df_non_cctv)),3)))
  print(paste0(round(mean(rowSums(cctv_df)),3), " - se - ", round(se(rowSums(cctv_df)),3)))
  
}

# -----------------------------------------------------


# Random points neighboring CCTV equipment

# Install the required packages
pkgs <- c('dplyr', 'data.table', 'ggplot2', 'ggpubr', 'tidyr', 'catboost', 'blockCV', 'caret', 'treeshap', 'stringr', 'sf')

if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkgs, rownames(installed.packages())))
}

# Load packages
lapply(pkgs, library, character.only = T)

# To sf data
transform_spat <- function(x){
  st_transform(st_as_sf(x), crs = paste0("epsg:", projection_used))
}

for (country in c("RU", 'FR', 'BE', "SC")) {
  
  {
    if (country == 'RU') { projection_used <- 32637 }
    if (country == 'SC') { projection_used <- 32630 }
    if (country == 'BE') { projection_used <- 31370 }
    if (country == 'FR') { projection_used <- 27561 }
  }
  
  cutoff <- 100
  # projection_used <- 4326
  df_sf <- readRDS(paste0("data/", country, "_buffer_long_", cutoff, "_RandSamp_GEOM.rds"))
  df_sf <- transform_spat(df_sf)
  
  rand_p <- df_sf %>% filter(is.na(cctv_ID))
  rand_p <- st_buffer(rand_p,100)
  
  cctv <- df_sf %>% 
    filter(!is.na(cctv_ID)) %>% 
    st_cast('MULTIPOINT') %>% 
    st_union()
  
  nearby_cctv <- unlist(st_intersects(rand_p, cctv))
  
  print(country)
  print(length(nearby_cctv)/10000)

}
