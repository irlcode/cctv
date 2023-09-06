########################################################
" Serebrennikov D., Skougarevskiy D. Tales of 3 cities "
"                  Code. Figure 1                      "
"       Spatial distribution of CCTV equipment         "
########################################################

# This code produces Figure 1



##################
## Preparations ## -------------------------------------
##################  

options(scipen = 999)

# Install the required packages
pkgs <- c("sf", "dplyr", "data.table", "mapview", "geojsonsf", "geojsonio", "geojson", "lubridate", "units", "stringi", "mapview", "stringr", "jsonlite", "jsonify", 'httr', 'ggrepel')

if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkgs, rownames(installed.packages())))
}

# Load packages
lapply(pkgs, library, character.only = T)

# Functions for correct coordinate transformation
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

# country = 'FR'

temp_list <- list()

for (country in c('RU', 'FR', 'BE', 'SC')) {


  setwd(paste0("data/", country, "/spat_data/data"))
  
  
  # District boundaries data load
  {
    if (country == 'RU') {
      district_boundaries <- st_read("boundary-polygon-lvl8.shp", stringsAsFactors = F)
      # Remove "New Moscow" because its territory joined to Moscow less than 10 years ago and its spatial morphology differs from 'Old' Moscow
      district_boundaries <- transform_spat(as.data.table(district_boundaries)[!ADMIN_L5 %in% c('Троицкий административный округ', 'Новомосковский административный округ')]) %>% 
        st_union()
      
    }
    if (country == 'SC') {
      district_boundaries <- st_read("boundary-polygon-lvl6.shp", stringsAsFactors = F)
      district_boundaries <- district_boundaries %>% 
        filter(NAME == "City of Edinburgh") %>% 
        st_union()
      
    }
    if (country == 'BE') {
      district_boundaries <- st_read("boundary-polygon-lvl4.shp", stringsAsFactors = F) 
      district_boundaries <- district_boundaries%>% 
        st_union()
      
    }
    if (country == 'FR') {
      district_boundaries <- st_read("boundary-polygon-lvl6.shp", stringsAsFactors = F)
      district_boundaries <- district_boundaries %>% 
        filter(NAME == "Paris") %>% 
        st_union()
      
    }
  }
  
  
  # We will make grid with camera presence. For it use geometries from population dataset
  # Load population data
  load(paste0("data/", country, "_pop_grid_500.Rdata"))
  pop_city_500 <- transform_spat(pop_city_500)
  # Load prepareded cctv data. 
  load(paste0("data/", country, "_cctv.rdata"))
  cctv <- transform_spat(cctv)
  
  # Attach CCCTV to grid data
  cctv$n <- 1
  cctv_grid <- st_join(pop_city_500,cctv[,c('n','geometry')], join = st_intersects, left = T)
  
  cctv_grid <- cctv_grid %>% 
    group_by(hex_id) %>% 
    summarise(cctv_n = sum(n)) %>% 
    mutate(cctv_n = ifelse(is.na(cctv_n), 0, cctv_n)) %>% 
    mutate(cctv_n = ifelse(cctv_n>2, 3, cctv_n)) # Make CCTV scale with four steps: 0, 1, 2, 3+
  
 
  # Make plot
  cctv_plot_grid <- ggplot()+  
    geom_sf(data=cctv_grid, aes(fill = cctv_n, color = cctv_n)) +
    geom_sf(data=district_boundaries, fill = NA, color = 'black')+
    scale_fill_gradient(low = "white", high = "#00BFC4", guide = F) +
    scale_color_gradient(low = "white", high = "#00BFC4", guide = F) +
    ggtitle("")+
    xlab("")+
    ylab("") +
    theme(legend.position = "none") +
    theme_void()+
    labs(caption = '© OpenStreetMap contributors')
  
  temp_list <- append(temp_list, list(cctv_plot_grid))
}


# Combine to plot

cctv_plot <- ggarrange(temp_list[[1]] + ggtitle('Moscow') + theme(plot.title = element_text(hjust = 0.5, size = 16)), 
                       temp_list[[2]] + ggtitle('Paris') + theme(plot.title = element_text(hjust = 0.5, vjust = 14.5, size = 16)),
                       temp_list[[3]] + ggtitle('Brussels') + theme(plot.title = element_text(hjust = 0.5, size = 16)),
                       temp_list[[4]] + ggtitle('Edinburgh') + theme(plot.title = element_text(hjust = 0.5, vjust = 5.5, size = 16)),
                       ncol=2, nrow=2, 
                       heights = c(4, 4), align = "v",
                       hjust=1, common.legend = TRUE, legend = "bottom")


# Save plot 
ggsave(plot = cctv_plot, paste0("output/cctv_city_plot.pdf"), width = 8, height = 9, scale = 1, dpi = 1000, units = "in") 

temp_list[[2]] + ggtitle('Paris') + theme(plot.title = element_text(hjust = 0.5, vjust =20))


# Save plot
#  ggsave(plot = cctv_plot_grid, paste0("output/fig_1_", country, ".png"), width = 4.5, height = 4.8, scale = 1, dpi = 300, units = "in")
  



