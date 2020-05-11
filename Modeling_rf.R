#Package installs -------------------------------------------------------------
load.fun <- function(x) { 
  x <- as.character(x) 
  if(isTRUE(x %in% .packages(all.available=TRUE))) { 
    eval(parse(text=paste("require(", x, ")", sep=""))) 
    print(paste(c(x, " : already installed; requiring"), collapse=''))
  } else { 
    #update.packages()
    print(paste(c(x, " : not installed; installing"), collapse=''))
    eval(parse(text=paste("install.packages('", x, "')", sep=""))) 
    print(paste(c(x, " : installed and requiring"), collapse=''))
    eval(parse(text=paste("require(", x, ")", sep=""))) 
  } 
} 

########### Required Packages ###########
packages = c("dplyr", "bayesplot", "lme4", "rstan", "shinystan", "RcppEigen",
             "tidyverse", "tidyr", "AmesHousing", "broom", "caret", "dials", "doParallel", "e1071", "earth",
             "ggrepel", "glmnet", "ipred", "klaR", "kknn", "pROC", "rpart", "randomForest",
             "sessioninfo", "tidymodels","ranger", "recipes", "workflows", "themis","xgboost",
             "sf", "nngeo", "mapview")

for(i in seq_along(packages)){
  packge <- as.character(packages[i])
  load.fun(packge)
}

session_info()

#########################################

#####with residential#####
#load data
complete <- read.csv("D:/Spring20/Practicum/data/with_route_fix_new.csv")
route <- read.csv("D:/Spring20/Practicum/data/route_fixed_effect.csv")
sce0 <- read.csv("D:/Spring20/Practicum/data/building_scenario0.csv")
nhood <- read.csv("D:/Spring20/Practicum/data/neighborhood.csv")
nhood_sf <- st_read("https://data.austintexas.gov/resource/nz5f-3t2e.geojson")%>%
  st_set_crs(4326)%>%
  st_transform(2278)
stops <- st_read("D:/Spring20/Practicum/data/Stops.shp")%>%
  st_transform(2278)
typology <- read.csv("D:/Spring20/Practicum/data/typology.csv")
complete <- left_join(complete, typology, by = "STOP_ID")
stop_sel <- c("STOP_ID", "geometry")
stops <- stops[stop_sel]

#route fixed effect
route_names <- colnames(subset(route, select = -c(r)))
complete_route <- complete%>%
  dplyr::select(STOP_ID,building_area, civic, commercial, residential, industrial, SN_cat, 
         Crosstown_cat, Express_cat, Local_cat, Flyer_cat, NightOwl_cat, 
         HighFreq_cat, InOut_cat,Clockwise_cat, hotline_1,utshuttle_cat, 
         Special_cat, school_count, stadium_count, medInc, count, population, mean_on,
         r7,r801,r10,r20,r803,r300,typology,label)
complete_nhood <- complete%>%
  dplyr::select(STOP_ID,building_area, civic, commercial, residential, industrial, SN_cat, 
                Crosstown_cat, Express_cat, Local_cat, Flyer_cat, NightOwl_cat, 
                HighFreq_cat, InOut_cat,Clockwise_cat, hotline_1,utshuttle_cat, 
                Special_cat, school_count, stadium_count, medInc, count, population, mean_on,
                r7,r801,r10,r20,r803,r300,label)

complete_route <- complete_route%>%
  drop_na()
write.csv(complete_route, "D:/Spring20/Practicum/data/JS_OG.csv")
complete_nhood <- complete_nhood%>%
  drop_na()
#
sce0<- left_join(sce0, nhood, by = "STOP_ID")

#drop NAs
sce0_ <- sce0 %>%
  drop_na()

set.seed(717)

theme_set(theme_bw())

write.csv(sce0_, "D:/Spring20/Practicum/data/bldg_sce0.csv")

### Initial Split for Training and Test
data_split_sce0 <- initial_split(complete_route, strata = "mean_on", prop = 0.75)

sce0_train <- training(data_split_sce0)
sce0_test  <- testing(data_split_sce0)


### Cross Validation
## LOGOCV on Neighborhood with group_vfold_cv()
cv_splits_geo_sce0 <- group_vfold_cv(sce0_train,  strata = "mean_on", group = "typology")

### Create Recipes

# Feature Creation
#Create recipe
model_rec_sce0 <- recipe(mean_on ~ ., data = complete_route) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_rm(STOP_ID,
          label)%>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #put on standard deviation scale

# See the data after all transformations
glimpse(model_rec_sce0 %>% prep() %>% juice())


## Model specifications
lm_plan <- 
  linear_reg() %>% 
  set_engine("lm")

glmnet_plan <- 
  linear_reg() %>% 
  set_args(penalty  = tune()) %>%
  set_args(mixture  = tune()) %>%
  set_engine("glmnet")

rf_plan <- rand_forest() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 1000) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

rf_plan1 <- rand_forest() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 100) %>% 
  set_engine("ranger", importance = "impurity", max.depth = 20) %>% 
  set_mode("regression")

XGB_plan <- boost_tree() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 100) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

# Hyperparameter grid for glmnet (penalization)
glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                           mixture = seq(0,1,0.25))
rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))
rf_grid1 <- expand.grid(mtry = c(2,5), 
                       min_n = c(2,5))
xgb_grid <- expand.grid(mtry = c(3,5), 
                        min_n = c(1,5))

# create workflow
lm_wf_sce0 <-
  workflow() %>% 
  add_recipe(model_rec_sce0) %>% 
  add_model(lm_plan)
glmnet_wf_sce0 <-
  workflow() %>% 
  add_recipe(model_rec_sce0) %>% 
  add_model(glmnet_plan)
rf_wf_sce0 <-
  workflow() %>% 
  add_recipe(model_rec_sce0) %>% 
  add_model(rf_plan)
rf_wf_sce0_1 <-
  workflow() %>% 
  add_recipe(model_rec_sce0) %>% 
  add_model(rf_plan1)
xgb_wf_sce0 <-
  workflow() %>% 
  add_recipe(model_rec_sce0) %>% 
  add_model(XGB_plan)

# fit model to workflow and calculate metrics
control <- control_resamples(save_pred = TRUE, verbose = TRUE)

lm_tuned_sce0 <- lm_wf_sce0 %>%
  tune::fit_resamples(.,
                      resamples = cv_splits_geo_sce0,
                      control   = control,
                      metrics   = metric_set(rmse, rsq))

glmnet_tuned_sce0 <- glmnet_wf_sce0 %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo_sce0,
                  grid      = glmnet_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

rf_tuned_sce0 <- rf_wf_sce0 %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo_sce0,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))
rf_tuned_sce0_1 <- rf_wf_sce0_1 %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo_sce0,
                  grid      = rf_grid1,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned_sce0 <- xgb_wf_sce0 %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo_sce0,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

lm_best_params_sce0     <- select_best(lm_tuned_sce0, metric = "rmse", maximize = FALSE)
glmnet_best_params_sce0 <- select_best(glmnet_tuned_sce0, metric = "rmse", maximize = FALSE)
rf_best_params_sce0     <- select_best(rf_tuned_sce0, metric = "rmse", maximize = FALSE)
rf_best_params_sce0_1     <- select_best(rf_tuned_sce0_1, metric = "rmse", maximize = FALSE)
xgb_best_params_sce0    <- select_best(xgb_tuned_sce0, metric = "rmse", maximize = FALSE)

## Final workflow
lm_best_wf_sce0     <- finalize_workflow(lm_wf_sce0, lm_best_params_sce0)
glmnet_best_wf_sce0 <- finalize_workflow(glmnet_wf_sce0, glmnet_best_params_sce0)
rf_best_wf_sce0     <- finalize_workflow(rf_wf_sce0, rf_best_params_sce0)
rf_best_wf_sce0_1     <- finalize_workflow(rf_wf_sce0_1, rf_best_params_sce0_1)
xgb_best_wf_sce0    <- finalize_workflow(xgb_wf_sce0, xgb_best_params_sce0)

# last_fit() emulates the process where, after determining the best model, the final fit on the entire training set is needed and is then evaluated on the test set.
lm_val_fit_geo_sce0 <- lm_best_wf_sce0 %>% 
  last_fit(split     = data_split_sce0,
           control   = control,
           metrics   = metric_set(rmse, rsq))

glmnet_val_fit_geo_sce0 <- glmnet_best_wf_sce0 %>% 
  last_fit(split     = data_split_sce0,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo_sce0 <- rf_best_wf_sce0 %>% 
  last_fit(split     = data_split_sce0,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo_sce0_1 <- rf_best_wf_sce0_1 %>% 
  last_fit(split     = data_split_sce0,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo_sce0 <- xgb_best_wf_sce0 %>% 
  last_fit(split     = data_split_sce0,
           control   = control,
           metrics   = metric_set(rmse, rsq))



# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds_sce0 <- collect_predictions(lm_tuned_sce0) 

glmnet_best_OOF_preds_sce0 <- collect_predictions(glmnet_tuned_sce0) %>% 
  filter(penalty  == glmnet_best_params_sce0$penalty[1] & mixture == glmnet_best_params_sce0$mixture[1])

rf_best_OOF_preds_sce0 <- collect_predictions(rf_tuned_sce0) %>% 
  filter(mtry  == rf_best_params_sce0$mtry[1] & min_n == rf_best_params_sce0$min_n[1])

xgb_best_OOF_preds_sce0 <- collect_predictions(xgb_tuned_sce0) %>% 
  filter(mtry  == xgb_best_params_sce0$mtry[1] & min_n == xgb_best_params_sce0$min_n[1])

# collect validation set predictions from last_fit model
lm_val_pred_geo_sce0     <- collect_predictions(lm_val_fit_geo_sce0)
glmnet_val_pred_geo_sce0 <- collect_predictions(glmnet_val_fit_geo_sce0)
rf_val_pred_geo_sce0     <- collect_predictions(rf_val_fit_geo_sce0)
xgb_val_pred_geo_sce0    <- collect_predictions(xgb_val_fit_geo_sce0)

# Aggregate OOF predictions (they do not overlap with Validation prediction set)
OOF_preds_sce0 <- rbind(data.frame(dplyr::select(lm_best_OOF_preds_sce0, .pred, mean_on), model = "lm"),
                   data.frame(dplyr::select(glmnet_best_OOF_preds_sce0, .pred, mean_on), model = "glmnet"),
                   data.frame(dplyr::select(rf_best_OOF_preds_sce0, .pred, mean_on), model = "RF"),
                   data.frame(dplyr::select(xgb_best_OOF_preds_sce0, .pred, mean_on), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup()

# average error for each model
ggplot(data = OOF_preds_sce0 %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  theme_bw()

# OOF predicted versus actual
ggplot(OOF_preds_sce0, aes(x =.pred, y = mean_on, group = model)) +
  geom_point(alpha = 0.3) +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  theme_bw()

# Aggregate predictions from Validation set
val_preds_sce0 <- rbind(data.frame(lm_val_pred_geo_sce0, model = "lm"),
                   data.frame(glmnet_val_pred_geo_sce0, model = "glmnet"),
                   data.frame(rf_val_pred_geo_sce0, model = "rf"),
                   data.frame(xgb_val_pred_geo_sce0, model = "xgb")) %>% 
  left_join(., complete_route %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  group_by(model) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup()

# plot MAPE by model type

ggplot(data = val_preds_sce0 %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_bar(stat = "identity",fill = palette4) +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title = "MAPE Comparisons")+
  plotTheme()



ggplot(data = val_preds_sce0 %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_bar(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title = "MAPE Comparisons")+
  plotTheme()

# average error for each model
ggplot(data = OOF_preds_sce0 %>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAE,1)))) +
  labs(title = "MAE Comparisons")+
  theme_bw()

ggplot(data = val_preds_sce0 %>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_bar(stat = "identity", fill = palette4) +
  geom_label(aes(label = paste0(round(MAE,1)))) +
  labs(title = "MAE Comparisons")+
  plotTheme()

# Validation Predicted vs. actual
ggplot(val_preds_sce0, aes(x =.pred, y = mean_on, group = model)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  labs(title = "Predicted vs.Observed on the Testing Set",
       subtitle = "blue line is predicted value")+
  plotTheme()

# join test data back to make spatial
val_pred_sf_sce0 <- left_join(val_preds_sce0, nhood_sf, by = "label")

# aggregate val error to Neighborhood 
val_MAPE_by_hood_sce0 <- val_pred_sf_sce0 %>% 
  group_by(label, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup()

val_MAPE_by_hood_sce0 <- left_join(val_MAPE_by_hood_sce0, nhood_sf, by = "label")%>%
  st_as_sf()

# plot MAPE by Hood
ggplot(filter(val_MAPE_by_hood_sce0, model == "rf") %>% 
         mutate(label = fct_reorder(label, MAPE)),
       aes(x = label, y = MAPE)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,100,5)) +
  labs(title = "MAPE by Neighborhoods")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0)
  )

# plot MAE by Hood
ggplot(filter(val_MAPE_by_hood_sce0, model == "rf") %>% 
         mutate(label = fct_reorder(label, MAE)),
       aes(x = label, y = MAE)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,200,5)) +
  labs(title = "MAE by Neighborhoods")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0)
  )

ggplot()+
  geom_sf(data = nhood_sf, aes(label = "label"))+
  geom_text()

#Map: MAPE of lm
val_MAPE_by_hood_sce0_lm <-val_MAPE_by_hood_sce0%>%
  filter(model=="lm") 

val_MAPE_by_hood_sce0_lm%>%
  ggplot() +
  geom_sf(data = rivers, color = "grey90", fill = "grey90")+
  geom_sf(data = streets1, color = "grey90")+
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(val_MAPE_by_hood_sce0_lm,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of lm in Neighborhoods") +
  coord_sf(xlim = c(2310627, 2390089), ylim = c(13980004, 14059914)) +
  mapTheme()

#Map: MAPE of glmnet
val_MAPE_by_hood_sce0_glmnet <-val_MAPE_by_hood_sce0%>%
  filter(model=="glmnet") 

val_MAPE_by_hood_sce0_glmnet%>%  
  ggplot() +
  geom_sf(data = rivers, color = "grey90", fill = "grey90")+
  geom_sf(data = streets1, color = "grey90")+
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(val_MAPE_by_hood_sce0_glmnet,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of glmnet in Neighborhoods") +
  coord_sf(xlim = c(2310627, 2390089), ylim = c(13980004, 14059914)) +
  mapTheme()

#MAPE of rf
val_MAPE_by_hood_sce0_rf <-val_MAPE_by_hood_sce0%>%
  filter(model=="rf") 

val_MAPE_by_hood_sce0_rf%>%  
  ggplot() +
  geom_sf(data = rivers, color = "grey90", fill = "grey90")+
  geom_sf(data = streets1, color = "grey90")+
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(val_MAPE_by_hood_sce0_rf,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of rf in Neighborhoods") +
  coord_sf(xlim = c(2310627, 2390089), ylim = c(13980004, 14059914)) +
  mapTheme()

val_MAPE_by_hood_sce0_rf%>%  
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(val_MAPE_by_hood_sce0_rf,"MAE"),
                    name="Quintile\nBreaks") +
  labs(title="MAE of rf in Neighborhoods") +
  mapTheme()

#MAPE of xgb
val_MAPE_by_hood_sce0_xgb <-val_MAPE_by_hood_sce0%>%
  filter(model=="xgb") 

val_MAPE_by_hood_sce0_xgb%>%
  ggplot() +
  geom_sf(data = rivers, color = "grey90", fill = "grey90")+
  geom_sf(data = streets1, color = "grey90")+
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(val_MAPE_by_hood_sce0_xgb,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of xgb in Neighborhoods") +
  coord_sf(xlim = c(2310627, 2390089), ylim = c(13980004, 14059914)) +
  mapTheme()

#####fit the full dataset to the model#####
#fit the model to best workflow
full_fit_model_rf0 <- fit(rf_best_wf_sce0, data = complete_route)
full_fit_model_xgb0 <- fit(xgb_best_wf_sce0, data = complete_route)

sce0_pred <- sce0_
sce0_pred$pred <- exp(predict(full_fit_model_rf0, sce0_))


#mueller
sce_m <- read.csv("D:/Spring20/Practicum/data/building_scenario3_eastdt.csv")

sce_mueller <- sce_m%>%
  drop_na()

sce_mueller<- left_join(sce_mueller, nhood, by = "STOP_ID")

sce_mueller$pred_rf <- exp(predict(full_fit_model_rf0, sce_mueller))

sce_mueller <- sce_mueller%>%
  mutate(diff = as.numeric(unlist(pred_rf_1)) - mean_on)

sce_mueller_sf <- left_join(sce_mueller, stops, by = "STOP_ID")

sce_mueller_sf <- sce_mueller_sf%>%
  st_as_sf()

ggplot()+
  geom_sf(data = sce_mueller_sf, aes(color = diff))

#westcampus
sce_w <- read.csv("D:/Spring20/Practicum/data/building_scenario1_westcampus.csv")

sce_west <- sce_w%>%
  drop_na()

sce_west<- left_join(sce_west, nhood, by = "STOP_ID")

sce_west$pred_rf <- exp(predict(full_fit_model_rf0, sce_west))

sce_west <- sce_west%>%
  mutate(diff = as.numeric(unlist(pred_rf)) - mean_on)

sce_west_sf <- left_join(sce_west, stops, by = "STOP_ID")

sce_west_sf <- sce_west_sf%>%
  st_as_sf()

ggplot()+
  geom_sf(data = sce_west_sf, aes(color = diff))+
  scale_color_gradient2(low = "darkred",
                       mid = "white",
                       high = "darkblue")

#westcampus
sce_r <- read.csv("D:/Spring20/Practicum/data/building_scenario2_riverside.csv")

sce_riverside <- sce_r%>%
  drop_na()

sce_riverside <- left_join(sce_riverside, nhood, by = "STOP_ID")

sce_riverside$pred_rf <- exp(predict(full_fit_model_rf0, sce_riverside))

sce_riverside <- sce_riverside%>%
  mutate(diff = as.numeric(unlist(pred_rf)) - mean_on)

#####landuse scenarios#####
# Housing development project East of DT
s1_lu1 <- c(829,5849,3281,954,1080,1078,956,3279,5508,4580,3278,3284,471,622,472,1417,620,2878,2880,2874,1275,1158,958,1077,4523,1076,960,1075,962,1074,963,1072,964,965,1070,966,1069,967,1068)
# make a copy
sce_lu1 <- complete_route

# Increase 640025 sqft
sce_lu1[sce_lu1$STOP_ID %in% s1_lu1,]$commercial <- sce_lu1[sce_lu1$STOP_ID %in% s1_lu1,]$commercial + 0.3
sce_lu1[sce_lu1$STOP_ID %in% s1_lu1,]$residential <- sce_lu1[sce_lu1$STOP_ID %in% s1_lu1,]$residential - 0.3
sce_lu1[sce_lu1$STOP_ID %in% s1_lu1,]$building_area <- sce_lu1[sce_lu1$STOP_ID %in% s1_lu1,]$building_area *2.5


######MedInc Scenario#####
sce_medinc <- complete_route

# Increase 640025 sqft
sce_medinc[sce_medinc$STOP_ID %in% s1_lu1,]$medinc <- sce_medinc[sce_medinc$STOP_ID %in% s1_lu1,]$medInc * 0.5


######Predict ridership for LU scenarios#####
sce_lu1$pred_rf <- exp(predict(full_fit_model_rf0, sce_lu1))

sce_lu1 <- sce_lu1%>%
  mutate(diff = as.numeric(unlist(pred_rf)) - mean_on)

sce_medinc$pred_rf <- exp(predict(full_fit_model_rf0, sce_medinc))

sce_medinc <- sce_medinc%>%
  mutate(diff = as.numeric(unlist(pred_rf)) - mean_on)

##### minus residential data due to correlation#####

library(corrplot)
cor_select <- c("building_area",
         "civic", "commercial","residential","industrial", "hotline_1","school_count","stadium_count","medInc","nshifts","mean_on")

sce0_cor <- sce0_[cor_select]

cor <- cor(sce0_cor)

corrplot(cor, type="upper", order="hclust")

#drop residential
sce_ <- sce0_
sce_$residential<- NULL

#####rf model coefficient#####
model_rf <- extract_model(full_fit_model_rf0)
model_xgb <- extract_model(full_fit_model_xgb0)
var_imp_sce0 <- as.data.frame(model_rf$variable.importance)
xgb_imp <- xgb.importance(model = model_xgb)

var_imp_sce0 <- tibble::rownames_to_column(var_imp_sce0, "VARIABLES")
var_imp_sce0 <- order(var_imp_sce0$VARIABLES)
var_imp_sce0 <- var_imp_sce0[order(var_imp_sce0$`model_rf$variable.importance`),]
rownames(var_imp_sce0) <- 1:nrow(var_imp_sce0)
ggplot(var_imp_sce0, aes(x = reorder(var_imp_sce0$VARIABLES, -var_imp_sce0$`model_rf$variable.importance`), y = var_imp_sce0$`model_rf$variable.importance`))+
  geom_col(aes(fill = var_imp_sce0$`model_rf$variable.importance`), width = 0.7)+
  xlab("Variables")+
  ylab("Feature Importance")+
  scale_fill_gradient2(name = "Feature Importance",
                       low = "deepskyblue", 
                       high = "dodgerblue4",
                       midpoint = median(var_imp_sce0$`model_rf$variable.importance`))+
  coord_flip()

write.csv(var_imp_sce0, "D:/Spring20/Practicum/data/var_imp_typology_new.csv")


#####morning_eve######
mor_eve <- read.csv("D:/Spring20/Practicum/data/new_morning_eve.csv")%>%
  drop_na()

library(MASS)
mor_eve_lm <- lm(morning_on ~ . - STOP_ID -mean_on -morning_off -morning_load -eve_on -eve_off -eve_load
                 -label -hubs -TRUSTEE, data = mor_eve)
step.model <- stepAIC(mor_eve_lm, direction = "both", 
                      trace = FALSE)
summary(step.model)
summary(mor_eve_lm)

mor_tweak <- lm(formula = morning_on ~ bar_count + university_count + school_count + 
     nshifts +  Counterclockwise + InOut + Express + 
     Feeder + Flyer + Local + Night.Owl + Special + 
     UT.Shuttle + hotline_1 + building_area  + 
     office + residential + race + population + 
     NoVeh + FiveVeh +  parkingDist + 
     airportDist + spatial_lag2 + 
     spatial_lag5 + Clockwise_cat + Counterclockwise_cat + Crosstown_cat + 
     Express_cat + Feeder_cat + Flyer_cat + HighFreq_cat + hotline_cat + 
     Local_cat + NightOwl_cat + SN_cat + Special_cat + WE_cat, 
   data = mor_eve)
summary(mor_tweak)

eve_lm <- lm(eve_on ~ . - STOP_ID -mean_on -morning_off -morning_load -morning_on -eve_off -eve_load
                 -label -hubs -TRUSTEE, data = mor_eve)
step.model.eve <- stepAIC(eve_lm, direction = "both", 
                      trace = FALSE)
summary(step.model.eve)

eve_tweak <- lm(formula = eve_on ~ retail_count + office_count + university_count + 
                  school_count + station_count + nshifts + Counterclockwise + 
                   Crosstown + Express + Flyer + Local + Special + 
                  hotline_1 + building_area + commercial + mixed_use + office + 
                  residential + race + population + spatial_lag5 + FiveVeh+
                  CBD_dist + parkingDist + stadiumDist + trainstationDist + 
                   Clockwise_cat + 
                  Counterclockwise_cat + Crosstown_cat + Express_cat + HighFreq_cat + 
                  hotline_cat + InOut_cat + Local_cat + SN_cat + Special_cat + 
                  WE_cat, data = mor_eve)

summary(eve_tweak)

#####model with nhood cv
### Initial Split for Training and Test
data_split_nhood <- initial_split(complete_nhood, strata = "mean_on", prop = 0.75)

nhood_train <- training(data_split_nhood)
nhood_test  <- testing(data_split_nhood)


### Cross Validation
## LOGOCV on Neighborhood with group_vfold_cv()
cv_splits_geo_nhood <- group_vfold_cv(nhood_train,  strata = "mean_on", group = "label")

### Create Recipes

# Feature Creation
#Create recipe
model_rec_nhood <- recipe(mean_on ~ ., data = complete_nhood) %>% #the "." means using every variable we have in the training dataset
  update_role(label, new_role = "label") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(label, threshold = 0.005) %>%
  step_rm(STOP_ID)%>%
  step_dummy(all_nominal(), -label) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #put on standard deviation scale


## Model specifications
lm_plan <- 
  linear_reg() %>% 
  set_engine("lm")

glmnet_plan <- 
  linear_reg() %>% 
  set_args(penalty  = tune()) %>%
  set_args(mixture  = tune()) %>%
  set_engine("glmnet")

rf_plan <- rand_forest() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 1000) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

XGB_plan <- boost_tree() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 100) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

# Hyperparameter grid for glmnet (penalization)
glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                           mixture = seq(0,1,0.25))
rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))
xgb_grid <- expand.grid(mtry = c(3,5), 
                        min_n = c(1,5))

# create workflow
lm_wf_nhood <-
  workflow() %>% 
  add_recipe(model_rec_nhood) %>% 
  add_model(lm_plan)
glmnet_wf_nhood <-
  workflow() %>% 
  add_recipe(model_rec_nhood) %>% 
  add_model(glmnet_plan)
rf_wf_nhood <-
  workflow() %>% 
  add_recipe(model_rec_nhood) %>% 
  add_model(rf_plan)
xgb_wf_nhood <-
  workflow() %>% 
  add_recipe(model_rec_nhood) %>% 
  add_model(XGB_plan)

# fit model to workflow and calculate metrics
control <- control_resamples(save_pred = TRUE, verbose = TRUE)

lm_tuned_nhood <- lm_wf_nhood %>%
  tune::fit_resamples(.,
                      resamples = cv_splits_geo_nhood,
                      control   = control,
                      metrics   = metric_set(rmse, rsq))

glmnet_tuned_nhood <- glmnet_wf_nhood %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo_nhood,
                  grid      = glmnet_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

rf_tuned_nhood <- rf_wf_nhood %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo_nhood,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned_nhood <- xgb_wf_nhood %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo_nhood,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

lm_best_params_nhood     <- select_best(lm_tuned_nhood, metric = "rmse", maximize = FALSE)
glmnet_best_params_nhood <- select_best(glmnet_tuned_nhood, metric = "rmse", maximize = FALSE)
rf_best_params_nhood     <- select_best(rf_tuned_nhood, metric = "rmse", maximize = FALSE)
xgb_best_params_nhood    <- select_best(xgb_tuned_nhood, metric = "rmse", maximize = FALSE)

## Final workflow
lm_best_wf_nhood     <- finalize_workflow(lm_wf_nhood, lm_best_params_nhood)
glmnet_best_wf_nhood <- finalize_workflow(glmnet_wf_nhood, glmnet_best_params_nhood)
rf_best_wf_nhood     <- finalize_workflow(rf_wf_nhood, rf_best_params_nhood)
xgb_best_wf_nhood    <- finalize_workflow(xgb_wf_nhood, xgb_best_params_nhood)

# last_fit() emulates the process where, after determining the best model, the final fit on the entire training set is needed and is then evaluated on the test set.
lm_val_fit_geo_nhood <- lm_best_wf_nhood %>% 
  last_fit(split     = data_split_nhood,
           control   = control,
           metrics   = metric_set(rmse, rsq))

glmnet_val_fit_geo_nhood <- glmnet_best_wf_nhood %>% 
  last_fit(split     = data_split_nhood,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo_nhood <- rf_best_wf_nhood %>% 
  last_fit(split     = data_split_nhood,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo_nhood <- xgb_best_wf_nhood %>% 
  last_fit(split     = data_split_nhood,
           control   = control,
           metrics   = metric_set(rmse, rsq))



# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds_nhood <- collect_predictions(lm_tuned_nhood) 

glmnet_best_OOF_preds_nhood <- collect_predictions(glmnet_tuned_nhood) %>% 
  filter(penalty  == glmnet_best_params_nhood$penalty[1] & mixture == glmnet_best_params_nhood$mixture[1])

rf_best_OOF_preds_nhood <- collect_predictions(rf_tuned_nhood) %>% 
  filter(mtry  == rf_best_params_nhood$mtry[1] & min_n == rf_best_params_nhood$min_n[1])

xgb_best_OOF_preds_nhood <- collect_predictions(xgb_tuned_nhood) %>% 
  filter(mtry  == xgb_best_params_nhood$mtry[1] & min_n == xgb_best_params_nhood$min_n[1])

# collect validation set predictions from last_fit model
lm_val_pred_geo_nhood     <- collect_predictions(lm_val_fit_geo_nhood)
glmnet_val_pred_geo_nhood <- collect_predictions(glmnet_val_fit_geo_nhood)
rf_val_pred_geo_nhood     <- collect_predictions(rf_val_fit_geo_nhood)
xgb_val_pred_geo_nhood    <- collect_predictions(xgb_val_fit_geo_nhood)

# Aggregate OOF predictions (they do not overlap with Validation prediction set)
OOF_preds_nhood <- rbind(data.frame(dplyr::select(lm_best_OOF_preds_nhood, .pred, mean_on), model = "lm"),
                        data.frame(dplyr::select(glmnet_best_OOF_preds_nhood, .pred, mean_on), model = "glmnet"),
                        data.frame(dplyr::select(rf_best_OOF_preds_nhood, .pred, mean_on), model = "RF"),
                        data.frame(dplyr::select(xgb_best_OOF_preds_nhood, .pred, mean_on), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup()

# average error for each model
ggplot(data = OOF_preds_nhood %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  theme_bw()

# OOF predicted versus actual
ggplot(OOF_preds_nhood, aes(x =.pred, y = mean_on, group = model)) +
  geom_point(alpha = 0.3) +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  theme_bw()

# Aggregate predictions from Validation set
val_preds_nhood <- rbind(data.frame(lm_val_pred_geo_nhood, model = "lm"),
                        data.frame(glmnet_val_pred_geo_nhood, model = "glmnet"),
                        data.frame(rf_val_pred_geo_nhood, model = "rf"),
                        data.frame(xgb_val_pred_geo_nhood, model = "xgb")) %>% 
  left_join(., complete_nhood %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(label, .row), 
            by = ".row") %>% 
  group_by(model) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup()

# plot MAPE by model type
ggplot(data = val_preds_nhood %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title = "MAPE Comparisons")+
  theme_bw()

# average error for each model
ggplot(data = OOF_preds_nhood %>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAE,1)))) +
  labs(title = "MAE Comparisons")+
  theme_bw()



# Validation Predicted vs. actual
ggplot(val_preds_nhood, aes(x =.pred, y = mean_on, group = model)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  theme_bw()

# join test data back to make spatial
val_pred_sf_nhood <- left_join(val_preds_nhood, nhood_sf, by = "label")

# aggregate val error to Neighborhood 
val_MAPE_by_hood_nhood <- val_pred_sf_nhood %>% 
  group_by(label, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup()

val_MAPE_by_hood_nhood <- left_join(val_MAPE_by_hood_nhood, nhood_sf, by = "label")%>%
  st_as_sf()

# plot MAPE by Hood
ggplot(filter(val_MAPE_by_hood_nhood, model == "rf") %>% 
         mutate(label = fct_reorder(label, MAPE)),
       aes(x = label, y = MAPE)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,100,5)) +
  labs(title = "MAPE by Neighborhoods")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0)
  )

# plot MAE by Hood
ggplot(filter(val_MAPE_by_hood_nhood, model == "rf") %>% 
         mutate(label = fct_reorder(label, MAE)),
       aes(x = label, y = MAE)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,200,5)) +
  labs(title = "MAE by Neighborhoods")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0)
  )

ggplot()+
  geom_sf(data = nhood_sf, aes(label = "label"))+
  geom_text()

#Map: MAPE of lm
val_MAPE_by_hood_nhood_lm <-val_MAPE_by_hood_nhood%>%
  filter(model=="lm") 

val_MAPE_by_hood_nhood_lm%>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(val_MAPE_by_hood_nhood_lm,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of lm in Neighborhoods") +
  mapTheme()

#Map: MAPE of glmnet
val_MAPE_by_hood_nhood_glmnet <-val_MAPE_by_hood_nhood%>%
  filter(model=="glmnet") 

val_MAPE_by_hood_nhood_glmnet%>%  
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(val_MAPE_by_hood_nhood_glmnet,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of glmnet in Neighborhoods") +
  mapTheme()
#MAPE of rf
val_MAPE_by_hood_nhood_rf <-val_MAPE_by_hood_nhood%>%
  filter(model=="rf") 

val_MAPE_by_hood_nhood_rf%>%  
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(val_MAPE_by_hood_nhood_rf,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of rf in Neighborhoods") +
  mapTheme()

val_MAPE_by_hood_nhood_rf%>%  
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(val_MAPE_by_hood_nhood_rf,"MAE"),
                    name="Quintile\nBreaks") +
  labs(title="MAE of rf in Neighborhoods") +
  mapTheme()

#MAPE of xgb
val_MAPE_by_hood_nhood_xgb <-val_MAPE_by_hood_nhood%>%
  filter(model=="xgb") 

val_MAPE_by_hood_nhood_xgb%>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(val_MAPE_by_hood_nhood_xgb,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of xgb in Neighborhoods") +
  mapTheme()

