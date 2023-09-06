########################################################
" Serebrennikov D., Skougarevskiy D. Tales of 4 cities "
"     Code. Part 1. Grid-Search & Cross-Validation     "
########################################################

# This code is used to conduct a search for the best model parameters, estimate them using cross-validation, and perform a final assessment of model quality.



##################
## Preparations ## -------------------------------------
##################  

set.seed(42)

# Load or install the required packages
pkgs <- c('dplyr', 'data.table', 'sf', 'ggplot2', 'ggpubr', 'tidyr', 'catboost', 'spatialsample', 'caret', 'treeshap', 'tidymodels', 'foreach', 'doParallel', 'themis')

if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkgs, rownames(installed.packages())))
}

# Load packages
lapply(pkgs, library, character.only = T)


#####################
###   FUNCTIONS   ### ----------------------------------
#####################

"%ni%" <- Negate("%in%")

# SE function
se <- function(x) round(sd(x)/sqrt(length(x)), 3)

# To sf data
transform_spat <- function(x){
  st_transform(st_as_sf(x), crs = paste0("epsg:", projection_used))
}

# Find best threshold for truth and predicted values
threshold_search <- function(truth, pred) {
  
  metrics_check <- data.frame()
  
  for (x in seq(0, 1, by = 0.01)) {
    
    tryCatch(
      expr = { 
        
        .pred_temp <- ifelse(pred > x, 1, 0)
        cf <- caret::confusionMatrix(factor(.pred_temp, levels=c('0','1')), 
                                     factor(truth, levels=c('0','1')),
                                     positive ='1', 
                                     mode="prec_recall")
        
        metrics_temp <- round(c(
                                x,
                                as.numeric(cf[["table"]]), 
                                cf[["byClass"]][["Precision"]],
                                cf[["byClass"]][["Recall"]],
                                cf[["byClass"]][["F1"]]),
                              3)
        
        metrics_check <- rbind(metrics_check, metrics_temp)
        
      },
      error = function(e){ 
        metrics_check <- rbind(metrics_check, c(x, rep(0,7)))
      }
    )
  }
  
  return(metrics_check)
}


############################
###   DATA PREPARATION   ### ----------------------------------
############################

country <- 'SC' # Possible choice: 'RU' (Moscow), 'FR' (Paris), 'BE' (Brussels), 'SC' (Edinburgh)
cutoff <- 100

# Select column which will be use for analysis
freqtab <- readRDS(paste0("data/", country, "_all_osm_objects_Freqtab.RDS"))
setDT(freqtab)
freqtab[,
        tag_name := gsub('shop_shop_', 'shop_', tag_name)]

col_interest <- c(freqtab[tag_full>49]$tag_name)
col_interest <- col_interest[col_interest %ni% c('bench','waste_basket','waste_disposal','vending_machine', 'post_box', 'yes', 'pole', "garden", "bar", 'pub', 'garage', 'shop_yes')]
col_interest <- col_interest[col_interest %ni% freqtab[grepl('shop', tag_name)][tag_full<100]$tag_name]


# Load main dataset
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

# Load geodata for spatial sample cross-validation
# Best projection for every city
{
  if (country == 'RU') { projection_used <- 32637 }
  if (country == 'SC') { projection_used <- 32630 }
  if (country == 'BE') { projection_used <- 31370 }
  if (country == 'FR') { projection_used <- 27561 }
}

df_sf <- readRDS(paste0("data/", country, "_buffer_long_", cutoff, "_RandSamp_GEOM.rds"))
df_sf <- transform_spat(df_sf)


########################
##     Modelling     ### ----------------------------------------
########################

# 1 Data splitting 
df_split <- initial_split(df, prop = 0.9, strata = y)
df_train <- training(df_split)
df_test  <- testing(df_split)

# 2. Add geometry data
df_train_4cv <- df_train %>%
  left_join(
    df_sf %>% select(common_ID, geometry),
    by = 'common_ID'
  ) %>% select(-common_ID)

df_train <- df_train[,-'common_ID']
df_test <- df_test[,-'common_ID']

# 2.2 Spatial CV resampling
spatial_cv_folds <- spatialsample::spatial_block_cv(transform_spat(df_train_4cv),
                                                    v = 5, 
                                                    repeats = 1)

# 3. Make grid for grid-search and cross-validation
grid_cb <- 
  grid_latin_hypercube(
    dials::parameters(
                      tree_depth(),
                      min_n()
                      ),
    size = 30)


# 4. Make cluster for parallel computation
cl <- makeCluster(4) #not to overload the computer
registerDoParallel(cl)

# 5. Make function to parallel the grid search
k_fold_CV_fun <- function(k, i) {
 
  tryCatch(
    expr = { 
      
      params <- list(
        iterations = 800,
        loss_function = 'Logloss', 
        depth = grid_cb[i,]$tree_depth,
        min_data_in_leaf = grid_cb[i,]$min_n,
        eval_metric='F1' 
      )
      
      k_train <- as.data.table(df_train)[spatial_cv_folds$splits[[k]]$in_id]
      k_test <- as.data.table(df_train)[!spatial_cv_folds$splits[[k]]$in_id]
      
      # Upsampling during CV (to prevent overoptimistic results)
      k_train_upsamp <- recipe(y ~ ., data = k_train %>% mutate(y = as.factor(y))) %>%
        step_upsample(y) %>%
        prep() %>% 
        juice() %>% 
        mutate(y = as.numeric(as.character(y)))
      
      setDT(k_train_upsamp)
      
      # Make pools 
      k_train_pool <- catboost.load_pool(data = k_train_upsamp[,-"y"], 
                                         label = k_train_upsamp$y)
      k_test_pool <- catboost.load_pool(data = k_test[,-"y"], 
                                        label = k_test$y)
      # Train
      model <- catboost.train(learn_pool = k_train_pool, params = params)
      y_pred <- catboost.predict(model, k_test_pool)
      y_pred_scaled <- (y_pred - min(y_pred)) / (max(y_pred) - min(y_pred))
      
      temp_metrics <- data.frame(i,
                                 k,
                                 threshold_search(k_test$y, y_pred_scaled)
                                 )

      colnames(temp_metrics) <- c('i', 'k', 'threshold', 'TN', 'FP', 'FN', 'TP', 'Precision', 'Recall', 'F1')
          
      return(temp_metrics)
  
      },
    error=function(e){cat("Error :",conditionMessage(e), "\n")}
  )
      
}


# 6. Grid-search and cross-validation
# WARNING! It can take a long time!
metrics_df <- data.frame()
metrics_df <- 
  foreach(i=1:nrow(grid_cb),
          .combine=rbind,
          .packages = c('data.table', 'tidymodels', 'catboost')) %:%
  foreach(k=1:5, 
          .combine=c,
          .packages = c('data.table', 'tidymodels', 'catboost')) %dopar% 
  k_fold_CV_fun(k,i) 

#stop cluster
stopCluster(cl)


# 7. Find the best parameters
# Prepare the data
setDT(metrics_df)

metrics_df_full <- data.frame()

for (mtr_r in 1:101) {
  temp <- metrics_df[mtr_r,]

  sq <- 1:30
  df_fin <- list()
  
  for (i in 1:50) {
    df_fin <- append(df_fin,
                    list(as.numeric(temp[,sq,with=F]))
                    )
    sq <- sq+30
  }
  
  df_fin <- df_fin %>% 
    unlist() %>% 
    matrix(ncol=30, byrow = T) %>% 
    t() 
  
  df_fin_1 <- rbind(
    df_fin[,1:10],
    df_fin[,11:20],
    df_fin[,21:30],
    df_fin[,31:40],
    df_fin[,41:50]
  ) %>% as.data.frame()

  colnames(df_fin_1) <- c('i', 'k', 'threshold', 'TN', 'FP', 'FN', 'TP', 'Precision', 'Recall', 'F1')
  
  metrics_df_full <- rbind(metrics_df_full, df_fin_1)
}

setDT(metrics_df_full)
metrics_top <- metrics_df_full[threshold > 0.3 & threshold < 0.7,
                     .(F1 = mean(F1),
                       F1_sd = sd(F1)),
                     by =.(i, k)]
metrics_top <- metrics_top[, 
                           .(F1 = mean(F1),
                             F1_sd = sd(F1)),
                           by =.(i)]
metrics_top[order(-F1)][1]

# Best parameters for every city:
# RU
grid_cb[2,]
i_fin <- 2

# FR
grid_cb[3,]
i_fin <- 3

# BE
grid_cb[5,]
i_fin <- 5

# SC
grid_cb[29,]
i_fin <- 29


# -----------------------------------------------

# 8. Final estimation
df_train_pool <- catboost.load_pool(data = df_train[,-"y"], 
                                   label = df_train$y)
df_test_pool <- catboost.load_pool(data = df_test[,-"y"], 
                                  label = df_test$y)
# Train
model_fin <- catboost.train(learn_pool = df_train_pool, 
                            params = list(
                              iterations = 800,
                              loss_function = 'Logloss', 
                              depth = grid_cb[i_fin,]$tree_depth,
                              min_data_in_leaf = grid_cb[i_fin,]$min_n,
                              eval_metric='F1' 
                            )
)

y_pred_fin <- catboost.predict(model_fin, df_test_pool)
y_pred_fin_scaled <- (y_pred_fin - min(y_pred_fin)) / (max(y_pred_fin) - min(y_pred_fin))

temp_metrics_fin <- threshold_search(df_test$y, y_pred_fin_scaled)
colnames(temp_metrics_fin) <- c('threshold', 'TN', 'FP', 'FN', 'TP', 'Precision', 'Recall', 'F1')

as.data.table(temp_metrics_fin)[order(-F1)][1]

# RU
#    threshold  TN  FP  FN  TP Precision Recall    F1
# 1:      0.56 811 189 113 465     0.711  0.804 0.755
# FR
#    threshold  TN FP FN TP Precision Recall    F1
# 1:      0.65 928 72 88 46      0.39  0.343 0.365

# BE
#    threshold  TN  FP FN TP Precision Recall    F1
# 1:      0.43 870 130 44 70      0.35  0.614 0.446
# SC
#    threshold  TN FP FN TP Precision Recall  F1
# 1:      0.61 985 18 10  6      0.25  0.375 0.3

# ----------------------------------------------------


# 9. Save the best parameters for SHAP analysis

# look manually for decide the best param
for (country in c('RU', "FR", "BE", "SC")) {

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
  
  # save best params
  if (country == "RU") { 
    tree_depth_t <- 9
    min_n_l <- 25
  }
  if (country == "SC") {
    tree_depth_t <- 14
    min_n_l <- 35
  }
  if (country == "FR") { 
    tree_depth_t <- 1
    min_n_l <- 23
  }  
  if (country == "BE") { 
    tree_depth_t <- 1
    min_n_l <- 7
  }  
  
  params <- list(
    iterations = 800,
    loss_function = 'Logloss',
    # rsm = rsm_t/ncol(df[,-c('y','common_ID')]),
    depth = tree_depth_t,
    # learning_rate = l_r,
    min_data_in_leaf = min_n_l,
    eval_metric='F1'
    )
  
  # Save
  saveRDS(params, 
          paste0("output/", country, "_", cutoff, "_catboost_best_params.rds"))
}







