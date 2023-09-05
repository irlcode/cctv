########################################################
" Serebrennikov D., Skougarevskiy D. Tales of 3 cities "
"              Code. Part 2. SHAP-Analysis             "
########################################################

# This code performs SHAP-analysis and creates plots based on it.


##################
## Preparations ## -------------------------------------
##################  

set.seed(42)

# Load or install the required packages
pkgs <- c('dplyr', 'data.table', 'ggplot2', 'ggpubr', 'tidyr', 'catboost', 'caret', 'treeshap', 'stringr')


if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkgs, rownames(installed.packages())))
}

# Load packages
lapply(pkgs, library, character.only = T)


#####################
###   FUNCTIONS   ### ---------------------------------------------------------
#####################

"%ni%" <- Negate("%in%")

# SE function

se <- function(x) round(sd(x)/sqrt(length(x)), 3)

#####################

# Get long data after catboost for Shapley analisys

get_shapley_data <- function(shap_tree_list, i) {
  
  data_temp <- shap_tree_list[[i]][[2]][["shaps"]]
  data_temp$index <- as.character(1:nrow(data_temp))
  data_temp <- melt(
    setDT(data_temp),
    id.vars = "index",
    value.name = "shap_effect_catboost",
    variable.name = "feature_name",
    variable.factor = F,
    na.rm = T
  )
  
  data_x <- shap_tree_list[[i]][[2]][["observations"]]
  data_x$index <- as.character(1:nrow(data_x))
  data_x <- melt(
    setDT(data_x),
    id.vars = "index",
    value.name = "real",
    variable.name = "feature_name",
    variable.factor = F,
    na.rm = T
  )
  
  data_y <- data.frame(
    index = as.character(1:length(shap_tree_list[[i]][[1]])),
    CCTV = ifelse(shap_tree_list[[i]][[1]] == 1, "Yes", "No")
  )
  data_y$CCTV <- factor(data_y$CCTV, levels = c("Yes", "No"))
  
  results <- merge(data_temp, data_x, by = c("index", "feature_name"), all = F)
  results <- merge(results, data_y, by = "index", all = F)
  
  return(results)
}

#####################

# Make errorbar plot 

make_errorbar_plot <- function(data, top_15_temp, city_temp = "", title_position = 0.5, legend_none = F, shap_labs = T, feat_value = F) {
  
  # setDT(data_all_final)
  # data <- data_all_final[city == "Moscow"]
  top_15_temp <- top_15_temp[,importance := shap] #[city == "Moscow", ]
  
  setDT(data)
  data[, real_fact := fcase(
    feature_name %in% c("distance", "population"), "" ,
    real == 1, "Yes",
    real == 0, "No"
  )]
  
  data <- data[,
               .(
                 mean_value = round(mean(shap_effect_catboost), 2),
                 se_value = se(shap_effect_catboost)
               ),
               by = .(feature_name, real_fact)]
  data <-  merge(data, top_15_temp[,c('feature_name', 'importance')], by ='feature_name', all.x=T)
  
  data[feature_name %in% c("distance", "population"), real_fact := NA_character_]
  data[, real_fact := factor(real_fact, levels = c("Yes", "No"))]
  
  data[, feature_name := str_to_title(feature_name)]
  data[, feature_name := gsub("_", " ", feature_name)]
  data[feature_name == 'Shop convenience',
                feature_name := 'Convenience Store']
  data[feature_name == 'Shop supermarket',
                feature_name := 'Supermarket']
  data[feature_name == 'Shop clothes',
                feature_name := 'Clothes shop']
  
  data[, feature_name := as.factor(feature_name)]
  data[, feature_name := forcats::fct_reorder(feature_name, -importance, .desc = T)]
  
  x_bound <- max(abs(data$mean_value + data$se_value)) #* 1.1
  
  if (shap_labs) {
    shap_labs_temp <- "Mean SHAP value - Log Odds" # (difference with average probability that an object is CCTV)
  } else {
    shap_labs_temp <- ""
  }
  
  plot <- ggplot() +
    geom_point(data = data,
               aes(
                 x = feature_name,
                 y = mean_value,
                 col = real_fact
               ), size = 2) +
    geom_errorbar(data = data,
                  aes(
                    x = feature_name,
                    y = mean_value,
                    ymin = mean_value-se_value,
                    ymax = mean_value+se_value,
                    col = real_fact
                  ),width = 0, size =1)+
    coord_flip(ylim = c(-x_bound, x_bound)) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw(base_size = 12) +
    scale_x_discrete(
      limits = levels(data$feature_name),
      labels = levels(data$feature_name)
    ) +
    scale_color_discrete(breaks = levels(data$real_fact)) +
    ggtitle(city_temp)+
    labs(y = shap_labs_temp, x = "", color='Amenity is in observation buffer:') +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      axis.text.y = element_text(hjust=0),
      plot.title = element_text(size = 14, hjust = title_position)
    ) 
  
  if (legend_none) {
    plot <- plot +
      theme(legend.position = "none")
  }
  
  if (feat_value) {
    plot <- plot +
      geom_text(data = data, aes(x = feature_name, y = mean_value, label = sprintf("%.2f", mean_value)), size = 2.8, alpha = 0.7, vjust = -0.9, hjust = 0.5) 
  }
  
  return(plot)
}



#######################
# DATA & CALCULATIONS # ---------------------------------------------------------
#######################

# Train full data on best metric and get SHAP-values from every model

cutoff <- 100
data_all <- list()
feature_importance <- list()

# Run for every city
for (country in c("RU", 'FR', "BE", "SC")) {
  
  freqtab <- readRDS(paste0("data/", country, "_all_osm_objects_Freqtab.RDS"))
  setDT(freqtab)
  freqtab[,
          tag_name := gsub('shop_shop_', 'shop_', tag_name)]
  
  col_interest <- c(freqtab[tag_full>49]$tag_name)
  col_interest <- col_interest[col_interest %ni% c('bench','waste_basket','waste_disposal','vending_machine', 'post_box', 'yes', 'pole', "garden", "bar", 'pub', 'garage', 'shop_yes')]
  col_interest <- col_interest[col_interest %ni% freqtab[grepl('shop', tag_name)][tag_full<100]$tag_name]
  
  
  # load full data
  all_objects <- fread(paste0("data/", country, "_buffer_wide_", cutoff, "_RandSamp.csv"), encoding = 'UTF-8')
  
  # Make y column
  all_objects[,
              y := ifelse(!is.na(cctv_ID),1,0)]
  
  # Prepare data
  df <- all_objects[, .SD,
                    .SDcols = c('common_ID', 'y', 'distance', 'population', 'bar_or_pub',col_interest)]
  df[,3:ncol(df)] <- lapply(df[,3:ncol(df)], as.numeric)
  
  # Good column names for df
  colnames(df) <- make.names(colnames(df))
  
  # Load parameters
  
  # Make pool for
  df_pool <- catboost.load_pool(data = df[,-c("y", "common_ID")], 
                                label = df$y)
  
  # Load best parameters for the model
  params <- readRDS(paste0("data/", country, "_", cutoff, "_catboost_best_params.rds"))
  
  # Learn
  model <- catboost.train(learn_pool = df_pool, 
                          params = params)
  
  # Get SHAP-values

    
  unified_catboost <- catboost.unify(model, df[,-c("y", "common_ID")])
  treeshap <- treeshap(unified_catboost, df[,-c("y", "common_ID")], verbose = 0)
  
  # data_temp <- catboost.get_feature_importance(model, 
  #                                              pool = df_pool, 
  #                                              type = "ShapValues")
  
  
  # Get feature importance
  shap_abs <- data.frame(
    names = colnames(df[,-c("y", "common_ID")]),
    shap = colMeans(data.frame(lapply(treeshap$shaps, abs)))
    )
  
  shap_abs_cctv <- data.frame(
    names = colnames(df[,-c("y", "common_ID")]),
    shap_cctv = colMeans(data.frame(lapply(treeshap$shaps[df$y==1,], abs)))
  )
  
  shap_abs_random <- data.frame(
    names = colnames(df[,-c("y", "common_ID")]),
    shap_random = colMeans(data.frame(lapply(treeshap$shaps[df$y==0,], abs)))
    )
  
  feat_import <- merge(
    shap_abs,
    shap_abs_cctv,
    by = 'names'
  )
  
  feat_import <- merge(
    feat_import,
    shap_abs_random,
    by = 'names'
  )
  
  # Append to lists
  feature_importance <- append(feature_importance, list(feat_import))
  
  data_all <- append(data_all, 
                     list(list(df$y,
                               treeshap)))
  
}

# Save
save(data_all, file = paste0("data/data_all_catboost_buffer_", cutoff, ".rdata"), compress = "xz")
save(feature_importance, file = paste0("data/feature_importance_catboost_buffer_", cutoff, ".rdata"), compress = "xz")

# load(paste0("data/data_all_catboost_buffer_", cutoff, ".rdata"))
# load(paste0("data/feature_importance_catboost_buffer_", cutoff, ".rdata"))


#####################
# Shapley analisys ## ---------------------------------------------------------
#####################

# Get top-15 feature importance 

top_15 <- data.frame()

for (i in 1:4) {
  temp <- cbind(i, feature_importance[[i]])
  temp <- head(temp[order(temp$shap, decreasing= T),], n = 15)
  top_15 <- rbind(top_15, temp)
}

setDT(top_15)

top_15[, city := fcase(
  i == 1, 'Moscow',
  i == 2, 'Paris',
  i == 3, 'Brussels',
  i == 4, 'Edinburgh'
)]

top_15[, shap := round(shap,2)]
top_15[, shap_cctv := round(shap_cctv,2)]
top_15[, shap_random := round(shap_random,2)]
setnames(top_15, 'names', 'feature_name')

# Make names clear
top_15[, feature_name := gsub('taxi', 'taxi_stop',feature_name)]
top_15[, feature_name := gsub('station', 'railway_station',feature_name)]
top_15[, feature_name := gsub('tram', 'tram_railway',feature_name)]

saveRDS(top_15, 
        paste0("data/catboost_SHAP_top_15_features.rdata"))

##### ---------------------------------------


# Make feature importance plot for every city in list
fi_with_labs <- "" # choose "_with_labs" or ""

# Make for feature importance plot for every city
temp_plot <- list()
for (cities in c('Moscow', 'Paris', 'Brussels', 'Edinburgh')) {
  shap_abs_temp <- top_15[city == cities]
  shap_abs_temp[, feature_name := str_to_title(feature_name)]
  shap_abs_temp[, feature_name := gsub("_", " ", feature_name)]
  shap_abs_temp[feature_name == 'Shop convenience',
                feature_name := 'Convenience store']
  shap_abs_temp[feature_name == 'Shop supermarket',
                feature_name := 'Supermarket']
  shap_abs_temp[feature_name == 'Shop clothes',
                feature_name := 'Clothes shop']

  shap_abs_temp[, feature_name := as.factor(feature_name)]
  shap_abs_temp[, feature_name := forcats::fct_reorder(feature_name, -shap, .desc = T)]

  temp <- shap_abs_temp %>%
    ggplot(aes(x=feature_name, y=shap)) +
    geom_bar(stat = "identity", fill = "#00BFC4") + 
    coord_flip() +
    theme_bw() +
    scale_x_discrete(
      limits = levels(shap_abs_temp$feature_name),
      labels = levels(shap_abs_temp$feature_name)
    ) +
    ggtitle(cities)+
    labs(y = 'mean(|SHAP value|)', x = "") +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.text.y = element_text(hjust=0),
      plot.title = element_text(hjust = 0.5)
    )
  if (fi_with_labs == "_with_labs") {
    temp <- temp +
      geom_text(data = shap_abs_temp, aes(x = feature_name, y = shap, label = sprintf("%.2f", shap)), size = 3, alpha = 0.7, vjust = 0.3, hjust = 1.3)
  }

  temp_plot <- append(temp_plot, list(temp))
}

# Combine
fi_plot <- ggarrange(temp_plot[[1]] + labs(y = ''), temp_plot[[2]] + labs(y = ''), temp_plot[[3]], temp_plot[[4]], ncol=2, nrow=2, hjust=1, common.legend = TRUE, legend = "bottom")

# Save plot
ggsave(plot = fi_plot, paste0("output/feat_imp_plot_buffer_", cutoff, fi_with_labs, ".pdf"), width = 8, height = 9, scale = 1, dpi = 800, units = "in") 

##### ---------------------------------------


# Make summary mean plots

# Get only top-15 feature observations for plots
data_all_final <- data.frame()
for (j in 1:4) {
  temp <- get_shapley_data(data_all, j)
  # Make names clear
  temp[, feature_name := gsub('taxi', 'taxi_stop',feature_name)]

  temp <- merge(temp,
                top_15[i == j, c("feature_name", 'city', 'shap')],
                by = "feature_name", 
                all=F)
  
  data_all_final <- rbind(data_all_final, temp)
}

# Choose resolution and lab value near points in plot 
cnum <- 2
rnum <- 2
with_labs_val <- F
if (with_labs_val) {
  with_labs <- '_with_labs'
  feat_value_ <- T
} else {
  with_labs <- ''
  feat_value_ <- F
  
}

# Make 4 plots for every city
mean_p1 <- make_errorbar_plot(data_all_final[city == "Moscow"],
                              top_15[city == "Moscow",],
                              city_temp = "Moscow",
                              legend_none = F,
                              feat_value = feat_value_,
                              shap_labs = F) 
mean_p2 <- make_errorbar_plot(data_all_final[city == "Paris"],
                              top_15[city == "Paris",],
                              city_temp = "Paris",
                              feat_value = feat_value_,
                              legend_none = F,
                              shap_labs = F) 
mean_p3 <- make_errorbar_plot(data_all_final[city == "Brussels"],
                              top_15[city == "Brussels",],
                              city_temp = "Brussels",
                              feat_value = feat_value_,
                              legend_none = F,
                              shap_labs = T) 
mean_p4 <- make_errorbar_plot(data_all_final[city == "Edinburgh"],
                              top_15[city == "Edinburgh",],
                              city_temp = "Edinburgh",
                              feat_value = feat_value_,
                              legend_none = F,
                              shap_labs = T) 

# Combine
mean_plot <- ggarrange(mean_p1, mean_p2, mean_p3, mean_p4, ncol=cnum, nrow=rnum, hjust=1, common.legend = TRUE, legend = "bottom")

# Save plot 
ggsave(plot = mean_plot, paste0("output/summary_mean_plot_buffer_", cutoff, "_", cnum,"x", rnum, with_labs, ".pdf"), width = 8, height = 9, scale = 1, dpi = 800, units = "in") # 5.9

##### ---------------------------------------


# Feature analisys - make distance plot
data_all_final[, City := factor(city, levels = c("Moscow", 'Paris', 'Brussels', "Edinburgh"))]

# Distance
lp1 <- data_all_final %>% 
  filter(real < 18000 & feature_name == "distance") %>% 
  group_by(city) %>% 
  mutate(shap_effect_catboost = scale(shap_effect_catboost)) %>% 
  ggplot() + 
  geom_smooth(
    mapping = aes(x = real, y = shap_effect_catboost, color = City),
    show.legend = T,
    span = 0.3,
    se=T,
    size = 1.1
  ) +
  ggtitle("Distances") +
  xlab("Meters from the city center") + 
  ylab("SHAP value - Log Odds") + 
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Combine
lp1 <- ggarrange(lp1, ncol=1, nrow=1, hjust=1, common.legend = TRUE, legend = "bottom")

# Save plot and data
ggsave(plot = lp1, paste0("output/distance_plot_buffer_", cutoff, ".pdf"), width = 8, height = 4.5, scale = 1, dpi = 800, units = "in") # 5.9


