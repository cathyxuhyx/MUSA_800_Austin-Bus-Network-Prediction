#####################
#   04/01 Modeling  #
#####################
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
packages = c("scales", "digest","colorspace","prodlim","yardstick","dplyr", "bayesplot", "lme4", "rstan", "shinystan", "RcppEigen",
             "tidyverse", "tidyr", "AmesHousing", "broom", "caret", "dials", "doParallel", "e1071", "earth",
             "ggrepel", "glmnet", "ipred", "klaR", "kknn", "pROC", "rpart", "randomForest",
             "sessioninfo", "tidymodels","ranger", "recipes", "workflows", "themis","xgboost",
             "sf", "nngeo", "mapview","parsnip", "vctrs", "rsample", "tune")

for(i in seq_along(packages)){
  packge <- as.character(packages[i])
  load.fun(packge)
} 

warnings()

library(dplyr)
library(bayesplot)
library(lme4)
library(rstan)
library(shinystan)
library(RcppEigen)
library(tidyverse)
library(broom)
library(ggplot2)
#Check NAs
sum(is.na(all_2))
which(is.na(all_2$Night.Owl))
#nshifts, Clockwise

#Create dataset without NAs
data.quarter <- all_2 %>%
  drop_na()

library(glmnet)
library(tidyr)

set.seed(717)

theme_set(theme_bw())

"%!in%" <- Negate("%in%")
g <- glimpse

data.quarter$STOP_ID<- NULL

#Slipt the data into training and testing sets
data_split <- rsample::initial_split(data, strata = "mean_on", prop = 0.75)

bus_train <- rsample::training(data_split)
bus_test  <- rsample::testing(data_split)
table(bus_train$label)
cv_splits_geo <- rsample::group_vfold_cv(bus_train,  strata = "mean_on", group = "label")
print(cv_splits_geo)

#Create recipe
model_rec <- recipe(mean_on ~ ., data = bus_train) %>% #the "." means using every variable we have in the training dataset
  update_role(label, new_role = "label") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(label, threshold = 0.005) %>%
  step_dummy(all_nominal(), -label) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
  #step_ns(Latitude, Longitude, options = list(df = 4))

model_rec

#Build the model
lm_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_engine("lm")

glmnet_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_args(penalty  = tune()) %>%
  parsnip::set_args(mixture  = tune()) %>%
  parsnip::set_engine("glmnet")

rf_plan <- parsnip::rand_forest() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 1000) %>% 
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("regression")

XGB_plan <- parsnip::boost_tree() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 100) %>% 
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("regression")

#
glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                           mixture = seq(0,1,0.25))

rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))
xgb_grid <- expand.grid(mtry = c(3,5), 
                        min_n = c(1,5))
#Create workflow
lm_wf <-
  workflows::workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(lm_plan)

glmnet_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(glmnet_plan)

rf_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(rf_plan)
xgb_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(XGB_plan)
# fit model to workflow and calculate metrics
control <- tune::control_resamples(save_pred = TRUE, verbose = TRUE)
library(tune)
library(yardstick)
?tune_grid
?metric_set

lm_tuned <- lm_wf %>%
  fit_resamples(.,
                resamples = cv_splits_geo,
                control   = control,
                metrics   = metric_set(rmse, rsq))
glmnet_tuned <- glmnet_wf %>%
  tune_grid(.,
            resamples = cv_splits_geo,
            grid      = glmnet_grid,
            control   = control,
            metrics   = metric_set(rmse, rsq))

rf_tuned <- rf_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned <- xgb_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

show_best(lm_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(rf_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(xgb_tuned, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params     <- select_best(lm_tuned, metric = "rmse", maximize = FALSE)
glmnet_best_params <- select_best(glmnet_tuned, metric = "rmse", maximize = FALSE)
rf_best_params     <- select_best(rf_tuned, metric = "rmse", maximize = FALSE)
xgb_best_params    <- select_best(xgb_tuned, metric = "rmse", maximize = FALSE)
#Final workflow
lm_best_wf     <- finalize_workflow(lm_wf, lm_best_params)
glmnet_best_wf <- finalize_workflow(glmnet_wf, glmnet_best_params)
rf_best_wf     <- finalize_workflow(rf_wf, rf_best_params)
xgb_best_wf    <- finalize_workflow(xgb_wf, xgb_best_params)

lm_val_fit_geo <- lm_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))
glmnet_val_fit_geo <- glmnet_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo <- rf_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo <- xgb_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

####################################Model Validation
# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds <- collect_predictions(lm_tuned) 
glmnet_best_OOF_preds <- collect_predictions(glmnet_tuned) %>% 
  filter(penalty  == glmnet_best_params$penalty[1] & mixture == glmnet_best_params$mixture[1])
rf_best_OOF_preds <- collect_predictions(rf_tuned) %>% 
  filter(mtry  == rf_best_params$mtry[1] & min_n == rf_best_params$min_n[1])

xgb_best_OOF_preds <- collect_predictions(xgb_tuned) %>% 
  filter(mtry  == xgb_best_params$mtry[1] & min_n == xgb_best_params$min_n[1])
# collect validation set predictions from last_fit model
lm_val_pred_geo     <- collect_predictions(lm_val_fit_geo)
glmnet_val_pred_geo <- collect_predictions(glmnet_val_fit_geo)
rf_val_pred_geo     <- collect_predictions(rf_val_fit_geo)
xgb_val_pred_geo    <- collect_predictions(xgb_val_fit_geo)
# Aggregate OOF predictions (they do not overlap with Validation prediction set)
OOF_preds <- rbind(data.frame(dplyr::select(lm_best_OOF_preds, .pred, mean_on), model = "lm"),
                   data.frame(dplyr::select(glmnet_best_OOF_preds, .pred, mean_on), model = "glmnet"),
                   data.frame(dplyr::select(rf_best_OOF_preds, .pred, mean_on), model = "RF"),
                   data.frame(dplyr::select(xgb_best_OOF_preds, .pred, mean_on), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup()


# Aggregate predictions from Validation set
val_preds <- rbind(data.frame(lm_val_pred_geo, model = "lm"),
                   data.frame(glmnet_val_pred_geo, model = "glmnet"),
                   data.frame(rf_val_pred_geo, model = "rf"),
                   data.frame(xgb_val_pred_geo, model = "xgb")) %>% 
  left_join(., data %>% 
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
###################################Modelling part ends here, below are the visualizations##############
#MAPE chart
ggplot(data = val_preds %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title= "MAPE of each model on the testing set")
  theme_bw()
#MAE chart
ggplot(data = val_preds %>% 
           dplyr::select(model, MAE) %>% 
           distinct() , 
         aes(x = model, y = MAE, group = 1)) +
    geom_path(color = "blue") +
    geom_label(aes(label = paste0(round(MAE,1)))) +
    labs(title= "MAE of each model on the testing set")
  theme_bw()  
  
#Predicted vs Observed
ggplot(val_preds, aes(x =.pred, y = mean_on, group = model)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  labs(title="Predicted vs Observed on the testing set", subtitle= "blue line is predicted value") 
  theme_bw()

#Neighborhood validation
val_MAPE_by_hood <- val_preds %>% 
  group_by(label, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup() 
#Barchart of the MAE in each neighborhood
plotTheme <- function(base_size = 20) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 80,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=60),
    axis.title = element_text(size=60),
    axis.text = element_text(size=45),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic", size= 80),
    legend.text = element_text(colour = "black", face = "italic",size = 80),
    strip.text.x = element_text(size = 50)
  )
}
palette4 <- c("#eff3ff", "#bdd7e7","#6baed6","#2171b5")


val_MAPE_by_hood %>%
  dplyr::select(label, model, MAE) %>%
  gather(Variable, MAE, -model, -label) %>%
  ggplot(aes(label, MAE)) + 
  geom_bar(aes(fill = model), position = "dodge", stat="identity") +
  scale_fill_manual(values = "Blues") +
  facet_wrap(~label,scales="free", ncol=6)+
  labs(title = "Mean Absolute Errors by model specification and neighborhood") +
  plotTheme()

#Map of MAE in each neighborhood
#Load neighborhood data
nhood <- st_read("https://data.austintexas.gov/resource/nz5f-3t2e.geojson")%>%
  st_set_crs(4326)%>%
  st_transform(2278)
#Add geometry to the MAE
MAE.nhood <- merge(nhood, val_MAPE_by_hood, by.x="label", by.y="label", all.y=TRUE)

#Produce the map
q5 <- function(variable) {as.factor(ntile(variable, 5))}
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}
library(RColorBrewer)
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = NA, fill=NA, size=2)
  )
}


#Map: MAPE of lm
MAE.nhood%>%
  filter(model=="lm") %>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(MAE.nhood,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of lm in Neighborhoods") +
  mapTheme()

#Map: MAPE of glmnet
MAE.nhood%>%
  filter(model=="glmnet") %>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(MAE.nhood,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of glmnet in Neighborhoods") +
  mapTheme()
#MAPE of rf
MAE.nhood%>%
  filter(model=="rf") %>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(MAE.nhood,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of rf in Neighborhoods") +
  mapTheme()

#MAPE of xgb
MAE.nhood%>%
  filter(model=="xgb") %>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(MAE.nhood,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of xgb in Neighborhoods") +
  mapTheme()









########################Test on school District##########
#Create recipe
model_rec <- recipe(mean_on ~ ., data = bus_train) %>% #the "." means using every variable we have in the training dataset
  update_role(TRUSTEE, new_role = "TRUSTEE") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(TRUSTEE, threshold = 0.005) %>%
  step_dummy(all_nominal(), -TRUSTEE) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))

model_rec

#Build the model
lm_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_engine("lm")

glmnet_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_args(penalty  = tune()) %>%
  parsnip::set_args(mixture  = tune()) %>%
  parsnip::set_engine("glmnet")

rf_plan <- parsnip::rand_forest() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 1000) %>% 
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("regression")

XGB_plan <- parsnip::boost_tree() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 100) %>% 
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("regression")

#
glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                           mixture = seq(0,1,0.25))

rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))
xgb_grid <- expand.grid(mtry = c(3,5), 
                        min_n = c(1,5))
#Create workflow
lm_wf <-
  workflows::workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(lm_plan)

glmnet_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(glmnet_plan)

rf_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(rf_plan)
xgb_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(XGB_plan)
# fit model to workflow and calculate metrics
control <- tune::control_resamples(save_pred = TRUE, verbose = TRUE)
library(tune)
library(yardstick)
?tune_grid
?metric_set

lm_tuned <- lm_wf %>%
  fit_resamples(.,
                resamples = cv_splits_geo,
                control   = control,
                metrics   = metric_set(rmse, rsq))
glmnet_tuned <- glmnet_wf %>%
  tune_grid(.,
            resamples = cv_splits_geo,
            grid      = glmnet_grid,
            control   = control,
            metrics   = metric_set(rmse, rsq))

rf_tuned <- rf_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned <- xgb_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

show_best(lm_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(rf_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(xgb_tuned, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params     <- select_best(lm_tuned, metric = "rmse", maximize = FALSE)
glmnet_best_params <- select_best(glmnet_tuned, metric = "rmse", maximize = FALSE)
rf_best_params     <- select_best(rf_tuned, metric = "rmse", maximize = FALSE)
xgb_best_params    <- select_best(xgb_tuned, metric = "rmse", maximize = FALSE)
#Final workflow
lm_best_wf     <- finalize_workflow(lm_wf, lm_best_params)
glmnet_best_wf <- finalize_workflow(glmnet_wf, glmnet_best_params)
rf_best_wf     <- finalize_workflow(rf_wf, rf_best_params)
xgb_best_wf    <- finalize_workflow(xgb_wf, xgb_best_params)

lm_val_fit_geo <- lm_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))
glmnet_val_fit_geo <- glmnet_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo <- rf_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo <- xgb_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

####################################Model Validation
# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds <- collect_predictions(lm_tuned) 
glmnet_best_OOF_preds <- collect_predictions(glmnet_tuned) %>% 
  filter(penalty  == glmnet_best_params$penalty[1] & mixture == glmnet_best_params$mixture[1])
rf_best_OOF_preds <- collect_predictions(rf_tuned) %>% 
  filter(mtry  == rf_best_params$mtry[1] & min_n == rf_best_params$min_n[1])

xgb_best_OOF_preds <- collect_predictions(xgb_tuned) %>% 
  filter(mtry  == xgb_best_params$mtry[1] & min_n == xgb_best_params$min_n[1])
# collect validation set predictions from last_fit model
lm_val_pred_geo     <- collect_predictions(lm_val_fit_geo)
glmnet_val_pred_geo <- collect_predictions(glmnet_val_fit_geo)
rf_val_pred_geo     <- collect_predictions(rf_val_fit_geo)
xgb_val_pred_geo    <- collect_predictions(xgb_val_fit_geo)
# Aggregate OOF predictions (they do not overlap with Validation prediction set)
OOF_preds <- rbind(data.frame(dplyr::select(lm_best_OOF_preds, .pred, mean_on), model = "lm"),
                   data.frame(dplyr::select(glmnet_best_OOF_preds, .pred, mean_on), model = "glmnet"),
                   data.frame(dplyr::select(rf_best_OOF_preds, .pred, mean_on), model = "RF"),
                   data.frame(dplyr::select(xgb_best_OOF_preds, .pred, mean_on), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup()


# Aggregate predictions from Validation set
val_preds <- rbind(data.frame(lm_val_pred_geo, model = "lm"),
                   data.frame(glmnet_val_pred_geo, model = "glmnet"),
                   data.frame(rf_val_pred_geo, model = "rf"),
                   data.frame(xgb_val_pred_geo, model = "xgb")) %>% 
  left_join(., data %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(TRUSTEE, .row), 
            by = ".row") %>% 
  group_by(model) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup()
###################################Modelling part ends here, below are the visualizations##############
#MAPE chart
ggplot(data = val_preds %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title= "MAPE of each model on the testing set")
theme_bw()
#MAE chart
ggplot(data = val_preds %>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAE,1)))) +
  labs(title= "MAE of each model on the testing set")
theme_bw()  

#Predicted vs Observed
ggplot(val_preds, aes(x =.pred, y = mean_on, group = model)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  labs(title="Predicted vs Observed on the testing set", subtitle= "blue line is predicted value") 
theme_bw()

#Neighborhood validation
val_MAPE_by_district <- val_preds %>% 
  group_by(TRUSTEE, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup() 
#Barchart of the MAE in each neighborhood
plotTheme <- function(base_size = 20) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 50,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=30),
    axis.title = element_text(size=30),
    axis.text = element_text(size=20),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic", size= 50),
    legend.text = element_text(colour = "black", face = "italic",size = 50),
    strip.text.x = element_text(size = 30)
  )
}


val_MAPE_by_district %>%
  dplyr::select(TRUSTEE, model, MAE) %>%
  gather(Variable, MAE, -model, -TRUSTEE) %>%
  ggplot(aes(TRUSTEE, MAE)) + 
  geom_bar(aes(fill = model), position = "dodge", stat="identity") +
  scale_fill_manual(values = palette4) +
  facet_wrap(~TRUSTEE,scales="free", ncol=8)+
  ylim(0,200)+
  labs(title = "Mean Absolute Errors by model specification and school districts") +
  plotTheme()

#Map of MAE in each school district
#Load school district data
TrusteeDist <- 
  st_read("C:/Upenn/Practicum/Data/Trustee_Boundaries/Trustee.shp")%>%
  st_transform(2278) 
#Add geometry to the MAE
MAE.district <- merge(TrusteeDist, val_MAPE_by_district, by.x="TRUSTEE", by.y="TRUSTEE", all.y=TRUE)

#Produce the map


#Map: MAPE of lm
MAE.district%>%
  filter(model=="lm") %>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(MAE.district,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of lm in School Districts") +
  mapTheme()

#Map: MAPE of glmnet
MAE.district%>%
  filter(model=="glmnet") %>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(MAE.district,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of glmnet in School Districts") +
  mapTheme()
#MAPE of rf
MAE.district%>%
  filter(model=="rf") %>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(MAE.district,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of rf in School Districts") +
  mapTheme()

#MAPE of xgb
MAE.district%>%
  filter(model=="xgb") %>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(MAE.district,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of xgb in School Districts") +
  mapTheme()

#####################################################################Modeling Improvement################################3
sum(is.na(all))
which(is.na(all))
which(is.na(all_2))
data2 <- all%>%
  drop_na()
#Slipt the data into training and testing sets
data_split2 <- rsample::initial_split(data2, strata = "mean_on", prop = 0.75)

bus_train2 <- rsample::training(data_split2)
bus_test2  <- rsample::testing(data_split2)
table(bus_train2$label)
cv_splits_geo <- rsample::group_vfold_cv(bus_train2,  strata = "mean_on", group = "label")
print(cv_splits_geo)

#Create recipe
model_rec <- recipe(mean_on ~ ., data = bus_train2) %>% #the "." means using every variable we have in the training dataset
  update_role(label, new_role = "label") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(label, threshold = 0.005) %>%
  step_dummy(all_nominal(), -label) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))

model_rec

#Build the model
lm_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_engine("lm")

glmnet_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_args(penalty  = tune()) %>%
  parsnip::set_args(mixture  = tune()) %>%
  parsnip::set_engine("glmnet")

rf_plan <- parsnip::rand_forest() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 1000) %>% 
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("regression")

XGB_plan <- parsnip::boost_tree() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 100) %>% 
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("regression")

#
glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                           mixture = seq(0,1,0.25))

rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))
xgb_grid <- expand.grid(mtry = c(3,5), 
                        min_n = c(1,5))
#Create workflow
lm_wf <-
  workflows::workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(lm_plan)

glmnet_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(glmnet_plan)

rf_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(rf_plan)
xgb_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(XGB_plan)
# fit model to workflow and calculate metrics
control <- tune::control_resamples(save_pred = TRUE, verbose = TRUE)
library(tune)
library(yardstick)
?tune_grid
?metric_set

lm_tuned <- lm_wf %>%
  fit_resamples(.,
                resamples = cv_splits_geo,
                control   = control,
                metrics   = metric_set(rmse, rsq))
glmnet_tuned <- glmnet_wf %>%
  tune_grid(.,
            resamples = cv_splits_geo,
            grid      = glmnet_grid,
            control   = control,
            metrics   = metric_set(rmse, rsq))

rf_tuned <- rf_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned <- xgb_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

show_best(lm_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(rf_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(xgb_tuned, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params     <- select_best(lm_tuned, metric = "rmse", maximize = FALSE)
glmnet_best_params <- select_best(glmnet_tuned, metric = "rmse", maximize = FALSE)
rf_best_params     <- select_best(rf_tuned, metric = "rmse", maximize = FALSE)
xgb_best_params    <- select_best(xgb_tuned, metric = "rmse", maximize = FALSE)
#Final workflow
lm_best_wf     <- finalize_workflow(lm_wf, lm_best_params)
glmnet_best_wf <- finalize_workflow(glmnet_wf, glmnet_best_params)
rf_best_wf     <- finalize_workflow(rf_wf, rf_best_params)
xgb_best_wf    <- finalize_workflow(xgb_wf, xgb_best_params)

lm_val_fit_geo <- lm_best_wf %>% 
  last_fit(split     = data_split2,
           control   = control,
           metrics   = metric_set(rmse, rsq))
glmnet_val_fit_geo <- glmnet_best_wf %>% 
  last_fit(split     = data_split2,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo <- rf_best_wf %>% 
  last_fit(split     = data_split2,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo <- xgb_best_wf %>% 
  last_fit(split     = data_split2,
           control   = control,
           metrics   = metric_set(rmse, rsq))

####################################Model Validation
# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds <- collect_predictions(lm_tuned) 
glmnet_best_OOF_preds <- collect_predictions(glmnet_tuned) %>% 
  filter(penalty  == glmnet_best_params$penalty[1] & mixture == glmnet_best_params$mixture[1])
rf_best_OOF_preds <- collect_predictions(rf_tuned) %>% 
  filter(mtry  == rf_best_params$mtry[1] & min_n == rf_best_params$min_n[1])

xgb_best_OOF_preds <- collect_predictions(xgb_tuned) %>% 
  filter(mtry  == xgb_best_params$mtry[1] & min_n == xgb_best_params$min_n[1])
# collect validation set predictions from last_fit model
lm_val_pred_geo     <- collect_predictions(lm_val_fit_geo)
glmnet_val_pred_geo <- collect_predictions(glmnet_val_fit_geo)
rf_val_pred_geo     <- collect_predictions(rf_val_fit_geo)
xgb_val_pred_geo    <- collect_predictions(xgb_val_fit_geo)
# Aggregate OOF predictions (they do not overlap with Validation prediction set)
OOF_preds <- rbind(data.frame(dplyr::select(lm_best_OOF_preds, .pred, mean_on), model = "lm"),
                   data.frame(dplyr::select(glmnet_best_OOF_preds, .pred, mean_on), model = "glmnet"),
                   data.frame(dplyr::select(rf_best_OOF_preds, .pred, mean_on), model = "RF"),
                   data.frame(dplyr::select(xgb_best_OOF_preds, .pred, mean_on), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup()


# Aggregate predictions from Validation set
val_preds <- rbind(data.frame(lm_val_pred_geo, model = "lm"),
                   data.frame(glmnet_val_pred_geo, model = "glmnet"),
                   data.frame(rf_val_pred_geo, model = "rf"),
                   data.frame(xgb_val_pred_geo, model = "xgb")) %>% 
  left_join(., data2 %>% 
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
###################################Modelling part ends here, below are the visualizations##############
#MAPE chart
ggplot(data = val_preds %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title= "MAPE of each model on the testing set")
theme_bw()
#MAE chart
ggplot(data = val_preds %>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAE,1)))) +
  labs(title= "MAE of each model on the testing set")
theme_bw()  

#Predicted vs Observed
ggplot(val_preds, aes(x =.pred, y = mean_on, group = model)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  labs(title="Predicted vs Observed on the testing set", subtitle= "blue line is predicted value") 
theme_bw()

#Neighborhood validation
val_MAPE_by_hood <- val_preds %>% 
  group_by(label, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup() 
#Barchart of the MAE in each neighborhood
plotTheme <- function(base_size = 20) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 80,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=60),
    axis.title = element_text(size=60),
    axis.text = element_text(size=45),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic", size= 80),
    legend.text = element_text(colour = "black", face = "italic",size = 80),
    strip.text.x = element_text(size = 50)
  )
}
palette4 <- c("#eff3ff", "#bdd7e7","#6baed6","#2171b5")


val_MAPE_by_hood %>%
  dplyr::select(label, model, MAE) %>%
  gather(Variable, MAE, -model, -label) %>%
  ggplot(aes(label, MAE)) + 
  geom_bar(aes(fill = model), position = "dodge", stat="identity") +
  scale_fill_manual(values = "Blues") +
  facet_wrap(~label,scales="free", ncol=6)+
  labs(title = "Mean Absolute Errors by model specification and neighborhood") +
  plotTheme()

#Map of MAE in each neighborhood
#Load neighborhood data
nhood <- st_read("https://data.austintexas.gov/resource/nz5f-3t2e.geojson")%>%
  st_set_crs(4326)%>%
  st_transform(2278)
#Add geometry to the MAE
MAE.nhood <- merge(nhood, val_MAPE_by_hood, by.x="label", by.y="label", all.y=TRUE)

#Produce the map
q5 <- function(variable) {as.factor(ntile(variable, 5))}
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}
library(RColorBrewer)
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = NA, fill=NA, size=2)
  )
}


#Map: MAPE of lm
MAE.nhood%>%
  filter(model=="lm") %>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(MAE.nhood,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of lm in Neighborhoods") +
  mapTheme()

#Map: MAPE of glmnet
MAE.nhood%>%
  filter(model=="glmnet") %>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(MAE.nhood,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of glmnet in Neighborhoods") +
  mapTheme()
#MAPE of rf
MAE.nhood%>%
  filter(model=="rf") %>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(MAE.nhood,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of rf in Neighborhoods") +
  mapTheme()

#MAPE of xgb
MAE.nhood%>%
  filter(model=="xgb") %>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(MAE.nhood,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of xgb in Neighborhoods") +
  mapTheme()














######################Try different buffers with typology########################################
#1/8-mile buffer
data.eighth <- join(all_eighth, typology, type ="left")
data.eighth$STOP_ID <- NULL

data.eighth<-data.eighth %>%
  drop_na()
data.eighth$universityDist1<-NULL
#Slipt the data into training and testing sets
data_split.eighth <- rsample::initial_split(data.eighth, strata = "mean_on", prop = 0.75)

bus_train.eighth <- rsample::training(data_split.eighth)
bus_test.eighth  <- rsample::testing(data_split.eighth)
names(bus_train.eighth)


cv_splits_geo.eighth <- rsample::group_vfold_cv(bus_train.eighth,  strata = "mean_on", group = "typology")
print(cv_splits_geo)

#Create recipe
model_rec.eighth <- recipe(mean_on ~ ., data = bus_train.eighth) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))
?step_cv
model_rec.eighth

#Build the model
lm_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_engine("lm")

glmnet_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_args(penalty  = tune()) %>%
  parsnip::set_args(mixture  = tune()) %>%
  parsnip::set_engine("glmnet")

rf_plan <- parsnip::rand_forest() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 1000) %>% 
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("regression")

XGB_plan <- parsnip::boost_tree() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 100) %>% 
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("regression")

#
glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                           mixture = seq(0,1,0.25))

rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))
xgb_grid <- expand.grid(mtry = c(3,5), 
                        min_n = c(1,5))
#Create workflow
lm_wf.eighth <-
  workflows::workflow() %>% 
  add_recipe(model_rec.eighth) %>% 
  add_model(lm_plan)

glmnet_wf.eighth <-
  workflow() %>% 
  add_recipe(model_rec.eighth) %>% 
  add_model(glmnet_plan)

rf_wf.eighth <-
  workflow() %>% 
  add_recipe(model_rec.eighth) %>% 
  add_model(rf_plan)
xgb_wf.eighth <-
  workflow() %>% 
  add_recipe(model_rec.eighth) %>% 
  add_model(XGB_plan)
# fit model to workflow and calculate metrics
control <- tune::control_resamples(save_pred = TRUE, verbose = TRUE)
library(tune)
library(yardstick)
?tune_grid
?metric_set

lm_tuned.eighth <- lm_wf.eighth %>%
  fit_resamples(.,
                resamples = cv_splits_geo.eighth,
                control   = control,
                metrics   = metric_set(rmse, rsq))
glmnet_tuned.eighth <- glmnet_wf.eighth %>%
  tune_grid(.,
            resamples = cv_splits_geo.eighth,
            grid      = glmnet_grid,
            control   = control,
            metrics   = metric_set(rmse, rsq))

rf_tuned.eighth <- rf_wf.eighth %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.eighth,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned.eighth <- xgb_wf.eighth %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.eighth,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

show_best(lm_tuned.eighth, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned.eighth, metric = "rmse", n = 15, maximize = FALSE)
show_best(rf_tuned.eighth, metric = "rmse", n = 15, maximize = FALSE)
show_best(xgb_tuned.eighth, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params.eighth     <- select_best(lm_tuned.eighth, metric = "rmse", maximize = FALSE)
glmnet_best_params.eighth <- select_best(glmnet_tuned.eighth, metric = "rmse", maximize = FALSE)
rf_best_params.eighth     <- select_best(rf_tuned.eighth, metric = "rmse", maximize = FALSE)
xgb_best_params.eighth    <- select_best(xgb_tuned.eighth, metric = "rmse", maximize = FALSE)
#Final workflow
lm_best_wf.eighth     <- finalize_workflow(lm_wf.eighth, lm_best_params.eighth)
glmnet_best_wf.eighth <- finalize_workflow(glmnet_wf.eighth, glmnet_best_params.eighth)
rf_best_wf.eighth     <- finalize_workflow(rf_wf.eighth, rf_best_params.eighth)
xgb_best_wf.eighth    <- finalize_workflow(xgb_wf.eighth, xgb_best_params.eighth)

lm_val_fit_geo.eighth <- lm_best_wf.eighth %>% 
  last_fit(split     = data_split.eighth,
           control   = control,
           metrics   = metric_set(rmse, rsq))
glmnet_val_fit_geo.eighth <- glmnet_best_wf.eighth %>% 
  last_fit(split     = data_split.eighth,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo.eighth <- rf_best_wf.eighth %>% 
  last_fit(split     = data_split.eighth,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo.eighth <- xgb_best_wf.eighth %>% 
  last_fit(split     = data_split.eighth,
           control   = control,
           metrics   = metric_set(rmse, rsq))

####################################Model Validation
# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds.eighth <- collect_predictions(lm_tuned.eighth) 
glmnet_best_OOF_preds.eighth <- collect_predictions(glmnet_tuned.eighth) %>% 
  filter(penalty  == glmnet_best_params.eighth$penalty[1] & mixture == glmnet_best_params.eighth$mixture[1])
rf_best_OOF_preds.eighth <- collect_predictions(rf_tuned.eighth) %>% 
  filter(mtry  == rf_best_params.eighth$mtry[1] & min_n == rf_best_params.eighth$min_n[1])

xgb_best_OOF_preds.eighth <- collect_predictions(xgb_tuned.eighth) %>% 
  filter(mtry  == xgb_best_params.eighth$mtry[1] & min_n == xgb_best_params.eighth$min_n[1])
# collect validation set predictions from last_fit model
lm_val_pred_geo.eighth     <- collect_predictions(lm_val_fit_geo.eighth)
glmnet_val_pred_geo.eighth <- collect_predictions(glmnet_val_fit_geo.eighth)
rf_val_pred_geo.eighth     <- collect_predictions(rf_val_fit_geo.eighth)
xgb_val_pred_geo.eighth    <- collect_predictions(xgb_val_fit_geo.eighth)
# Aggregate OOF predictions (they do not overlap with Validation prediction set)
OOF_preds.eighth <- rbind(data.frame(dplyr::select(lm_best_OOF_preds.eighth, .pred, mean_on), model = "lm"),
                           data.frame(dplyr::select(glmnet_best_OOF_preds.eighth, .pred, mean_on), model = "glmnet"),
                           data.frame(dplyr::select(rf_best_OOF_preds.eighth, .pred, mean_on), model = "RF"),
                           data.frame(dplyr::select(xgb_best_OOF_preds.eighth, .pred, mean_on), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         #RSQUARE = yardstick::rsq(mean_on, .pred),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         #SD_RMSE = sd(yardstick::rmse_vec(mean_on, .pred)),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         #SD_MAE = sd(yardstick::mae_vec(mean_on, .pred)),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  #SD_MAPE = sd(yardstick::mape_vec(mean_on, .pred))) %>% 
  ungroup()


# Aggregate predictions from Validation set
library(tidyverse)
library(yardstick)
#lm_val_pred_geo
detach(package:plyr)
val_preds.eighth <- rbind(data.frame(lm_val_pred_geo.eighth, model = "lm"),
                   data.frame(glmnet_val_pred_geo.eighth, model = "glmnet"),
                   data.frame(rf_val_pred_geo.eighth, model = "rf"),
                   data.frame(xgb_val_pred_geo.eighth, model = "xgb")) %>% 
  left_join(., data.eighth %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  group_by(model) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = mean(abs(mean_on - .pred)),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()


lm_val_pred_geo.eighth<- lm_val_pred_geo.eighth%>%
  left_join(., data.eighth %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row")%>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on))

d %>%
  group_by(Name) %>%
  summarise_at(vars(-Month), funs(mean(., na.rm=TRUE)))

lm_val_pred_geo.eighth%>%
  group_by(typology)%>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         Error = abs(mean_on, .pred),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         #MAE  = yardstick::mae_vec(mean_on, .pred),
         MAE = mean(Error),
         MAPE = yardstick::mape_vec(mean_on, .pred))

glmnet_val_pred_geo.eighth<- glmnet_val_pred_geo.eighth%>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))

rf_val_pred_geo.eighth<- rf_val_pred_geo.eighth%>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))

xgb_val_pred_geo.eighth<- xgb_val_pred_geo.eighth%>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))

val_preds.eighth <- rbind(data.frame(lm_val_pred_geo.eighth, model = "lm"),
                           data.frame(glmnet_val_pred_geo.eighth, model = "glmnet"),
                           data.frame(rf_val_pred_geo.eighth, model = "rf"),
                           data.frame(xgb_val_pred_geo.eighth, model = "xgb"))%>%
  left_join(., data.eighth %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row")
summary(lm_val_pred_geo.eighth$MAE)
summary(glmnet_val_pred_geo.eighth$MAE)
summary(rf_val_pred_geo.eighth$MAE)
summary(xgb_val_pred_geo.eighth$MAE)
summary(lm_val_pred_geo.eighth$MAPE)
summary(glmnet_val_pred_geo.eighth$MAPE)
summary(rf_val_pred_geo.eighth$MAPE)
summary(xgb_val_pred_geo.eighth$MAPE)
summary(lm_val_pred_geo.eighth$RMSE)
summary(glmnet_val_pred_geo.eighth$RMSE)
summary(rf_val_pred_geo.eighth$RMSE)
summary(xgb_val_pred_geo.eighth$RMSE)
?group_by
#Rsquared
1- sum((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred) ^ 2)/sum((lm_val_pred_geo$mean_on - mean(lm_val_pred_geo$mean_on)) ^ 2)
rsq(lm_val_pred_geo.eighth, mean_on, .pred)
sd(rsq(lm_val_pred_geo, mean_on, .pred))
rsq(glmnet_val_pred_geo.eighth, mean_on, .pred)
rsq(rf_val_pred_geo.eighth, mean_on, .pred)
rsq(xgb_val_pred_geo.eighth, mean_on, .pred)
#MAE and MAPE
mean(abs(lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred))
sd(abs(lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred))
mean(abs((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)/lm_val_pred_geo$mean_on))
sd(abs((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)/lm_val_pred_geo$mean_on))
sd(abs(lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred))

mean(abs(glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred))
sd(abs(glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred))
mean(abs((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)/glmnet_val_pred_geo$mean_on))
sd(abs((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)/glmnet_val_pred_geo$mean_on))

mean(abs(rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred))
sd(abs(rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred))
mean(abs((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)/rf_val_pred_geo$mean_on))
sd(abs((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)/rf_val_pred_geo$mean_on))

mean(abs(xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred))
sd(abs(xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred))
mean(abs((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)/xgb_val_pred_geo$mean_on))
sd(abs((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)/xgb_val_pred_geo$mean_on))
#RMSE
sqrt(mean((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)^2))
sqrt(mean((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)^2))
sqrt(mean((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)^2))
sqrt(mean((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)^2))
sqrt(sd((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)^2))
sqrt(sd((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)^2))
sqrt(sd((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)^2))
sqrt(sd((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)^2))

yardstick::rmse_vec(lm_val_pred_geo$mean_on, lm_val_pred_geo$.pred)
yardstick::mape_vec(lm_val_pred_geo$mean_on, lm_val_pred_geo$.pred)
###################################Modelling part ends here, below are the visualizations##############
#MAPE chart
ggplot(data = val_preds.eighth %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title= "1/8-mi Buffer, MAPE of each model on the testing set with typology")
theme_bw()
#MAE chart
ggplot(data = val_preds.eighth%>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAE,1)))) +
  labs(title= "1/8 mi Buffer, MAE of each model on the testing set with typology")
theme_bw()  
#RMSE
ggplot(data = val_preds.eighth %>% 
         dplyr::select(model, RMSE) %>% 
         distinct() , 
       aes(x = model, y = RMSE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(RMSE,1)))) +
  labs(title= "1/8 mi Buffer, RMSE of each model on the testing set with typology")
theme_bw() 
#Predicted vs Observed
ggplot(val_preds.eighth, aes(x =.pred, y = mean_on, group = model)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  labs(title="1/8 Mile: Predicted vs Observed on the testing set", subtitle= "blue line is predicted value") 
theme_bw()

val_MAPE_by_typology.eighth <- val_preds.eighth %>% 
  group_by(typology, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup() 
#Barchart of the MAE in each neighborhood
plotTheme <- function(base_size = 10) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 20,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text = element_text(size=15),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic", size= 20),
    legend.text = element_text(colour = "black", face = "italic",size = 20),
    strip.text.x = element_text(size = 15)
  )
}
palette4 <- c("#eff3ff", "#bdd7e7","#6baed6","#2171b5")


as.data.frame(val_MAPE_by_typology.eighth)%>%
  dplyr::select(typology, model, MAE) %>%
  #gather(Variable, MAE, -model, -typology) %>%
  ggplot(aes(typology, MAE)) + 
  geom_bar(aes(fill = model), position = "dodge", stat="identity") +
  ylim(0, 300)+
  scale_fill_manual(values = palette4) +
  facet_wrap(~typology, scale= "free", ncol=4)+
  labs(title = "1/8 mile: Mean Absolute Errors by model specification") +
  plotTheme()





#1/2 Buffer Size with Typology Test
data.half <- join(all_half, typology, type ="left")
data.half$STOP_ID <- NULL
data.half<-data.half %>%
  drop_na()
data.half$universityDist1<-NULL
#Slipt the data into training and testing sets
data_split.half <- rsample::initial_split(data.half, strata = "mean_on", prop = 0.75)

bus_train.half <- rsample::training(data_split.half)
bus_test.half  <- rsample::testing(data_split.h)
names(bus_train.half)


cv_splits_geo.half <- rsample::group_vfold_cv(bus_train.half,  strata = "mean_on", group = "typology")

#Create recipe
model_rec.half <- recipe(mean_on ~ ., data = bus_train.half) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))
?step_cv
model_rec.half

#Build the model
lm_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_engine("lm")

glmnet_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_args(penalty  = tune()) %>%
  parsnip::set_args(mixture  = tune()) %>%
  parsnip::set_engine("glmnet")

rf_plan <- parsnip::rand_forest() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 1000) %>% 
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("regression")

XGB_plan <- parsnip::boost_tree() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 100) %>% 
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("regression")

#
glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                           mixture = seq(0,1,0.25))

rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))
xgb_grid <- expand.grid(mtry = c(3,5), 
                        min_n = c(1,5))
#Create workflow
lm_wf.half <-
  workflows::workflow() %>% 
  add_recipe(model_rec.half) %>% 
  add_model(lm_plan)

glmnet_wf.half <-
  workflow() %>% 
  add_recipe(model_rec.half) %>% 
  add_model(glmnet_plan)

rf_wf.half <-
  workflow() %>% 
  add_recipe(model_rec.half) %>% 
  add_model(rf_plan)
xgb_wf.half <-
  workflow() %>% 
  add_recipe(model_rec.half) %>% 
  add_model(XGB_plan)
# fit model to workflow and calculate metrics
control <- tune::control_resamples(save_pred = TRUE, verbose = TRUE)
library(tune)
library(yardstick)
?tune_grid
?metric_set

lm_tuned.half <- lm_wf.half %>%
  fit_resamples(.,
                resamples = cv_splits_geo.half,
                control   = control,
                metrics   = metric_set(rmse, rsq))
glmnet_tuned.half <- glmnet_wf.half %>%
  tune_grid(.,
            resamples = cv_splits_geo.half,
            grid      = glmnet_grid,
            control   = control,
            metrics   = metric_set(rmse, rsq))

rf_tuned.half <- rf_wf.half %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.half,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned.half <- xgb_wf.half %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.half,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

show_best(lm_tuned.half, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned.half, metric = "rmse", n = 15, maximize = FALSE)
show_best(rf_tuned.half, metric = "rmse", n = 15, maximize = FALSE)
show_best(xgb_tuned.half, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params.half     <- select_best(lm_tuned.half, metric = "rmse", maximize = FALSE)
glmnet_best_params.half <- select_best(glmnet_tuned.half, metric = "rmse", maximize = FALSE)
rf_best_params.half     <- select_best(rf_tuned.half, metric = "rmse", maximize = FALSE)
xgb_best_params.half    <- select_best(xgb_tuned.half, metric = "rmse", maximize = FALSE)
#Final workflow
lm_best_wf.half     <- finalize_workflow(lm_wf.half, lm_best_params.half)
glmnet_best_wf.half <- finalize_workflow(glmnet_wf.half, glmnet_best_params.half)
rf_best_wf.half     <- finalize_workflow(rf_wf.half, rf_best_params.half)
xgb_best_wf.half    <- finalize_workflow(xgb_wf.half, xgb_best_params.half)

lm_val_fit_geo.half <- lm_best_wf.half %>% 
  last_fit(split     = data_split.half,
           control   = control,
           metrics   = metric_set(rmse, rsq))
glmnet_val_fit_geo.half <- glmnet_best_wf.half %>% 
  last_fit(split     = data_split.half,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo.half <- rf_best_wf.half %>% 
  last_fit(split     = data_split.half,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo.half <- xgb_best_wf.half %>% 
  last_fit(split     = data_split.half,
           control   = control,
           metrics   = metric_set(rmse, rsq))

####################################Model Validation
# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds.half <- collect_predictions(lm_tuned.half) 
glmnet_best_OOF_preds.half <- collect_predictions(glmnet_tuned.half) %>% 
  filter(penalty  == glmnet_best_params.half$penalty[1] & mixture == glmnet_best_params.half$mixture[1])
rf_best_OOF_preds.half <- collect_predictions(rf_tuned.half) %>% 
  filter(mtry  == rf_best_params.half$mtry[1] & min_n == rf_best_params.half$min_n[1])

xgb_best_OOF_preds.half <- collect_predictions(xgb_tuned.half) %>% 
  filter(mtry  == xgb_best_params.half$mtry[1] & min_n == xgb_best_params.half$min_n[1])
# collect validation set predictions from last_fit model
lm_val_pred_geo.half     <- collect_predictions(lm_val_fit_geo.half)
glmnet_val_pred_geo.half <- collect_predictions(glmnet_val_fit_geo.half)
rf_val_pred_geo.half     <- collect_predictions(rf_val_fit_geo.half)
xgb_val_pred_geo.half    <- collect_predictions(xgb_val_fit_geo.half)
# Aggregate OOF predictions (they do not overlap with Validation prediction set)
OOF_preds.half <- rbind(data.frame(dplyr::select(lm_best_OOF_preds.half, .pred, mean_on), model = "lm"),
                           data.frame(dplyr::select(glmnet_best_OOF_preds.half, .pred, mean_on), model = "glmnet"),
                           data.frame(dplyr::select(rf_best_OOF_preds.half, .pred, mean_on), model = "RF"),
                           data.frame(dplyr::select(xgb_best_OOF_preds.half, .pred, mean_on), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         #RSQUARE = yardstick::rsq(mean_on, .pred),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         #SD_RMSE = sd(yardstick::rmse_vec(mean_on, .pred)),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         #SD_MAE = sd(yardstick::mae_vec(mean_on, .pred)),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  #SD_MAPE = sd(yardstick::mape_vec(mean_on, .pred))) %>% 
  ungroup()


# Aggregate predictions from Validation set
library(tidyverse)
library(yardstick)
#lm_val_pred_geo
val_preds.half <- rbind(data.frame(lm_val_pred_geo.half, model = "lm"),
                   data.frame(glmnet_val_pred_geo.half, model = "glmnet"),
                   data.frame(rf_val_pred_geo.half, model = "rf"),
                   data.frame(xgb_val_pred_geo.half, model = "xgb")) %>% 
  left_join(., data.half %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(model) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()


summary(lm_val_pred_geo.half$MAE)
summary(glmnet_val_pred_geo.half$MAE)
summary(rf_val_pred_geo.half$MAE)
summary(xgb_val_pred_geo.half$MAE)
summary(lm_val_pred_geo.half$MAPE)
summary(glmnet_val_pred_geo.half$MAPE)
summary(rf_val_pred_geo.half$MAPE)
summary(xgb_val_pred_geo.half$MAPE)
summary(lm_val_pred_geo.half$RMSE)
summary(glmnet_val_pred_geo.half$RMSE)
summary(rf_val_pred_geo.half$RMSE)
summary(xgb_val_pred_geo.half$RMSE)
?group_by
#Rsquared
1- sum((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred) ^ 2)/sum((lm_val_pred_geo$mean_on - mean(lm_val_pred_geo$mean_on)) ^ 2)
rsq(lm_val_pred_geo.half, mean_on, .pred)
sd(rsq(lm_val_pred_geo, mean_on, .pred))
rsq(glmnet_val_pred_geo.half, mean_on, .pred)
rsq(rf_val_pred_geo.half, mean_on, .pred)
rsq(xgb_val_pred_geo.half, mean_on, .pred)
#MAE and MAPE
mean(abs(lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred))
sd(abs(lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred))
mean(abs((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)/lm_val_pred_geo$mean_on))
sd(abs((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)/lm_val_pred_geo$mean_on))
sd(abs(lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred))

mean(abs(glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred))
sd(abs(glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred))
mean(abs((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)/glmnet_val_pred_geo$mean_on))
sd(abs((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)/glmnet_val_pred_geo$mean_on))

mean(abs(rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred))
sd(abs(rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred))
mean(abs((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)/rf_val_pred_geo$mean_on))
sd(abs((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)/rf_val_pred_geo$mean_on))

mean(abs(xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred))
sd(abs(xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred))
mean(abs((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)/xgb_val_pred_geo$mean_on))
sd(abs((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)/xgb_val_pred_geo$mean_on))
#RMSE
sqrt(mean((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)^2))
sqrt(mean((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)^2))
sqrt(mean((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)^2))
sqrt(mean((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)^2))
sqrt(sd((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)^2))
sqrt(sd((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)^2))
sqrt(sd((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)^2))
sqrt(sd((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)^2))

yardstick::rmse_vec(lm_val_pred_geo$mean_on, lm_val_pred_geo$.pred)
yardstick::mape_vec(lm_val_pred_geo$mean_on, lm_val_pred_geo$.pred)
###################################Modelling part ends here, below are the visualizations##############
#MAPE chart
ggplot(data = val_preds.half %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title= "1/2-mi Buffer, MAPE of each model on the testing set with typology")
theme_bw()
#MAE chart
ggplot(data = val_preds.half%>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAE,1)))) +
  labs(title= "1/2 mi Buffer, MAE of each model on the testing set with typology")
theme_bw()  
#RMSE
ggplot(data = val_preds.half%>% 
         dplyr::select(model, RMSE) %>% 
         distinct() , 
       aes(x = model, y = RMSE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(RMSE,1)))) +
  labs(title= "1/2 mi Buffer, RMSE of each model on the testing set with typology")
theme_bw() 
#Predicted vs Observed
ggplot(val_preds.half, aes(x =.pred, y = mean_on, group = model)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  labs(title="1/2 Mile: Predicted vs Observed on the testing set", subtitle= "blue line is predicted value") 
theme_bw()

#Neighborhood validation
val_MAPE_by_typology.half <- val_preds.half %>% 
  group_by(typology, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup() 
#Barchart of the MAE in each neighborhood
plotTheme <- function(base_size = 10) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 20,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text = element_text(size=15),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic", size= 20),
    legend.text = element_text(colour = "black", face = "italic",size = 20),
    strip.text.x = element_text(size = 15)
  )
}
palette4 <- c("#eff3ff", "#bdd7e7","#6baed6","#2171b5")


as.data.frame(val_MAPE_by_typology.half)%>%
  dplyr::select(typology, model, MAE) %>%
  #gather(Variable, MAE, -model, -typology) %>%
  ggplot(aes(typology, MAE)) + 
  geom_bar(aes(fill = model), position = "dodge", stat="identity") +
  ylim(0, 300)+
  scale_fill_manual(values = palette4) +
  facet_wrap(~typology, scale= "free", ncol=4)+
  labs(title = "1/2 mile: Mean Absolute Errors by model specification") +
  plotTheme()









#1/4 mile buffer, typology test
#1/4 mile buffer with ridership: data.quarter
#typology
sum(is.na(All_3))
sum(is.na(all_2))
All_3$typology
summary(all_2$parkingDist)
typology<- All_3 %>%
  dplyr::select(STOP_ID, typology)
typology$typology <- ifelse(typology$typology == "CBD" , 'CBD',
                                ifelse(typology$typology == "UT", 'UT',
                                       ifelse(typology$typology == "UT&CBD", 'CBD', 'Rest')))

#write.csv(typology, "C:/Upenn/Practicum/Data/Typology_withSTOP_ID.csv")
?join
names(all_2)
data.quarter <- plyr::join(all_2, typology, type ="left")
data.quarter$STOP_ID <- NULL

data.quarter<-data.quarter %>%
  drop_na()
data.quarter$universityDist1<-NULL
#Slipt the data into training and testing sets
data_split.quarter <- rsample::initial_split(data.quarter, strata = "mean_on", prop = 0.75)

bus_train.quarter <- rsample::training(data_split.quarter)
bus_test.quarter  <- rsample::testing(data_split.quarter)
names(bus_train.quarter)


cv_splits_geo.quarter <- rsample::group_vfold_cv(bus_train.quarter,  strata = "mean_on", group = "typology")
print(cv_splits_geo)

#Create recipe
model_rec.quarter <- recipe(mean_on ~ ., data = bus_train.quarter) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))
?step_cv
model_rec.quarter

#Build the model
lm_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_engine("lm")

glmnet_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_args(penalty  = tune()) %>%
  parsnip::set_args(mixture  = tune()) %>%
  parsnip::set_engine("glmnet")

rf_plan <- parsnip::rand_forest() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 1000) %>% 
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("regression")

XGB_plan <- parsnip::boost_tree() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 100) %>% 
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("regression")

#
glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                           mixture = seq(0,1,0.25))

rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))
xgb_grid <- expand.grid(mtry = c(3,5), 
                        min_n = c(1,5))
#Create workflow
lm_wf.quarter <-
  workflows::workflow() %>% 
  add_recipe(model_rec.quarter) %>% 
  add_model(lm_plan)

glmnet_wf.quarter <-
  workflow() %>% 
  add_recipe(model_rec.quarter) %>% 
  add_model(glmnet_plan)

rf_wf.quarter <-
  workflow() %>% 
  add_recipe(model_rec.quarter) %>% 
  add_model(rf_plan)
xgb_wf.quarter <-
  workflow() %>% 
  add_recipe(model_rec.quarter) %>% 
  add_model(XGB_plan)
# fit model to workflow and calculate metrics
control <- tune::control_resamples(save_pred = TRUE, verbose = TRUE)
library(tune)
library(yardstick)
?tune_grid
?metric_set

lm_tuned.quarter <- lm_wf.quarter %>%
  fit_resamples(.,
                resamples = cv_splits_geo.quarter,
                control   = control,
                metrics   = metric_set(rmse, rsq))
glmnet_tuned.quarter <- glmnet_wf.quarter %>%
  tune_grid(.,
            resamples = cv_splits_geo.quarter,
            grid      = glmnet_grid,
            control   = control,
            metrics   = metric_set(rmse, rsq))

rf_tuned.quarter <- rf_wf.quarter %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.quarter,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned.quarter <- xgb_wf.quarter %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.quarter,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

show_best(lm_tuned.quarter, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned.quarter, metric = "rmse", n = 15, maximize = FALSE)
show_best(rf_tuned.quarter, metric = "rmse", n = 15, maximize = FALSE)
show_best(xgb_tuned.quarter, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params.quarter     <- select_best(lm_tuned.quarter, metric = "rmse", maximize = FALSE)
glmnet_best_params.quarter <- select_best(glmnet_tuned.quarter, metric = "rmse", maximize = FALSE)
rf_best_params.quarter     <- select_best(rf_tuned.quarter, metric = "rmse", maximize = FALSE)
xgb_best_params.quarter    <- select_best(xgb_tuned.quarter, metric = "rmse", maximize = FALSE)
#Final workflow
lm_best_wf.quarter     <- finalize_workflow(lm_wf.quarter, lm_best_params.quarter)
glmnet_best_wf.quarter <- finalize_workflow(glmnet_wf.quarter, glmnet_best_params.quarter)
rf_best_wf.quarter     <- finalize_workflow(rf_wf.quarter, rf_best_params.quarter)
xgb_best_wf.quarter    <- finalize_workflow(xgb_wf.quarter, xgb_best_params.quarter)

lm_val_fit_geo.quarter <- lm_best_wf.quarter %>% 
  last_fit(split     = data_split.quarter,
           control   = control,
           metrics   = metric_set(rmse, rsq))
glmnet_val_fit_geo.quarter <- glmnet_best_wf.quarter %>% 
  last_fit(split     = data_split.quarter,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo.quarter <- rf_best_wf.quarter %>% 
  last_fit(split     = data_split.quarter,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo.quarter <- xgb_best_wf.quarter %>% 
  last_fit(split     = data_split.quarter,
           control   = control,
           metrics   = metric_set(rmse, rsq))

####################################Model Validation
# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds.quarter <- collect_predictions(lm_tuned.quarter) 
glmnet_best_OOF_preds.quarter <- collect_predictions(glmnet_tuned.quarter) %>% 
  filter(penalty  == glmnet_best_params.quarter$penalty[1] & mixture == glmnet_best_params.quarter$mixture[1])
rf_best_OOF_preds.quarter <- collect_predictions(rf_tuned.quarter) %>% 
  filter(mtry  == rf_best_params.quarter$mtry[1] & min_n == rf_best_params.quarter$min_n[1])

xgb_best_OOF_preds.quarter <- collect_predictions(xgb_tuned.quarter) %>% 
  filter(mtry  == xgb_best_params.quarter$mtry[1] & min_n == xgb_best_params.quarter$min_n[1])
# collect validation set predictions from last_fit model
lm_val_pred_geo.quarter     <- collect_predictions(lm_val_fit_geo.quarter)
glmnet_val_pred_geo.quarter <- collect_predictions(glmnet_val_fit_geo.quarter)
rf_val_pred_geo.quarter     <- collect_predictions(rf_val_fit_geo.quarter)
xgb_val_pred_geo.quarter    <- collect_predictions(xgb_val_fit_geo.quarter)
# Aggregate OOF predictions (they do not overlap with Validation prediction set)
lm_best_OOF_preds$mean_on <- as.numeric(lm_best_OOF_preds$mean_on)
glmnet_best_OOF_preds$mean_on <- as.numeric(glmnet_best_OOF_preds$mean_on)
rf_best_OOF_preds$mean_on <- as.numeric(rf_best_OOF_preds$mean_on)
xgb_best_OOF_preds$mean_on <- as.numeric(xgb_best_OOF_preds$mean_on)

OOF_preds.quarter <- rbind(data.frame(dplyr::select(lm_best_OOF_preds.quarter, .pred, mean_on), model = "lm"),
                   data.frame(dplyr::select(glmnet_best_OOF_preds.quarter, .pred, mean_on), model = "glmnet"),
                   data.frame(dplyr::select(rf_best_OOF_preds.quarter, .pred, mean_on), model = "RF"),
                   data.frame(dplyr::select(xgb_best_OOF_preds.quarter, .pred, mean_on), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         #RSQUARE = yardstick::rsq(mean_on, .pred),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         #SD_RMSE = sd(yardstick::rmse_vec(mean_on, .pred)),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         #SD_MAE = sd(yardstick::mae_vec(mean_on, .pred)),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
         #SD_MAPE = sd(yardstick::mape_vec(mean_on, .pred))) %>% 
  ungroup()


# Aggregate predictions from Validation set
library(tidyverse)
library(yardstick)
#lm_val_pred_geo
val_preds.quarter <- rbind(data.frame(lm_val_pred_geo.quarter, model = "lm"),
                   data.frame(glmnet_val_pred_geo.quarter, model = "glmnet"),
                   data.frame(rf_val_pred_geo.quarter, model = "rf"),
                   data.frame(xgb_val_pred_geo.quarter, model = "xgb")) %>% 
  left_join(., data.quarter %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(model) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()


summary(rf_val_pred_geo.quarter$MAE)
summary(xgb_val_pred_geo.quarter$MAE)
summary(lm_val_pred_geo.quarter$MAPE)
summary(glmnet_val_pred_geo.quarter$MAPE)
summary(rf_val_pred_geo.quarter$MAPE)
summary(xgb_val_pred_geo.quarter$MAPE)
summary(lm_val_pred_geo.quarter$RMSE)
summary(glmnet_val_pred_geo.quarter$RMSE)
summary(rf_val_pred_geo.quarter$RMSE)
summary(xgb_val_pred_geo.quarter$RMSE)
?group_by
#Rsquared
1- sum((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred) ^ 2)/sum((lm_val_pred_geo$mean_on - mean(lm_val_pred_geo$mean_on)) ^ 2)
rsq(lm_val_pred_geo.quarter, mean_on, .pred)
sd(rsq(lm_val_pred_geo, mean_on, .pred))
rsq(glmnet_val_pred_geo.quarter, mean_on, .pred)
rsq(rf_val_pred_geo.quarter, mean_on, .pred)
rsq(xgb_val_pred_geo.quarter, mean_on, .pred)
#MAE and MAPE
mean(abs(lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred))
sd(abs(lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred))
mean(abs((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)/lm_val_pred_geo$mean_on))
sd(abs((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)/lm_val_pred_geo$mean_on))
sd(abs(lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred))

mean(abs(glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred))
sd(abs(glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred))
mean(abs((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)/glmnet_val_pred_geo$mean_on))
sd(abs((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)/glmnet_val_pred_geo$mean_on))

mean(abs(rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred))
sd(abs(rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred))
mean(abs((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)/rf_val_pred_geo$mean_on))
sd(abs((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)/rf_val_pred_geo$mean_on))

mean(abs(xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred))
sd(abs(xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred))
mean(abs((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)/xgb_val_pred_geo$mean_on))
sd(abs((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)/xgb_val_pred_geo$mean_on))
#RMSE
sqrt(mean((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)^2))
sqrt(mean((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)^2))
sqrt(mean((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)^2))
sqrt(mean((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)^2))
sqrt(sd((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)^2))
sqrt(sd((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)^2))
sqrt(sd((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)^2))
sqrt(sd((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)^2))

yardstick::rmse_vec(lm_val_pred_geo$mean_on, lm_val_pred_geo$.pred)
yardstick::mape_vec(lm_val_pred_geo$mean_on, lm_val_pred_geo$.pred)
###################################Modelling part ends here, below are the visualizations##############
#MAPE chart
ggplot(data = val_preds.quarter %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title= "1/4-mi Buffer, MAPE of each model on the testing set with typology")
theme_bw()
#MAE chart
ggplot(data = val_preds.quarter%>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAE,1)))) +
  labs(title= "1/4 mi Buffer, MAE of each model on the testing set with typology")
theme_bw()  
#RMSE
ggplot(data = val_preds.quarter %>% 
         dplyr::select(model, RMSE) %>% 
         distinct() , 
       aes(x = model, y = RMSE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(RMSE,1)))) +
  labs(title= "1/4 mi Buffer, RMSE of each model on the testing set with typology")
theme_bw() 
#Predicted vs Observed
ggplot(val_preds.quarter, aes(x =.pred, y = mean_on, group = model)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  labs(title="1/4 Mile: Predicted vs Observed on the testing set", subtitle= "blue line is predicted value") 
theme_bw()

#Neighborhood validation
val_MAPE_by_typology.quarter <- val_preds.quarter %>% 
  group_by(typology, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup() 
#Barchart of the MAE in each neighborhood
plotTheme <- function(base_size = 10) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 20,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text = element_text(size=15),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic", size= 20),
    legend.text = element_text(colour = "black", face = "italic",size = 20),
    strip.text.x = element_text(size = 15)
  )
}
palette4 <- c("#eff3ff", "#bdd7e7","#6baed6","#2171b5")


as.data.frame(val_MAPE_by_typology.quarter)%>%
  dplyr::select(typology, model, MAE) %>%
  #gather(Variable, MAE, -model, -typology) %>%
  ggplot(aes(typology, MAE)) + 
  geom_bar(aes(fill = model), position = "dodge", stat="identity") +
  ylim(0, 300)+
  scale_fill_manual(values = palette4) +
  facet_wrap(~typology, scale= "free", ncol=4)+
  labs(title = "1/4 mile: Mean Absolute Errors by model specification") +
  plotTheme()

as.data.frame(val_MAPE_by_typology.quarter)%>%
  dplyr::select(typology, model, MAPE) %>%
  #gather(Variable, MAE, -model, -typology) %>%
  ggplot(aes(typology, MAPE)) + 
  geom_bar(aes(fill = model), position = "dodge", stat="identity") +
  #ylim(0, 300)+
  scale_fill_manual(values = palette4) +
  facet_wrap(~typology, scale= "free", ncol=4)+
  labs(title = "1/4 mile: MAPE by model specification") +
  plotTheme()

summary(lm(mean_on ~ ., data = data.quarter %>% 
             select(c(commercial,residential,office,commercial_count, residential_count, supermkt_count,
                      university_count,school_count,mean_on)) ) )
#others,nature,civic,industrial,transportation,mixed_use, ,office_count,retail_count,station_count,stadium_count,bar_count,,parking_count



#Models for ensemble
#Land use model
#select(c(building_area,commercial,residential,civic,transportation,industrial,label,mean_on))
data.lu <- data.quarter%>%
  dplyr::select(building_area,commercial,residential,civic,transportation,industrial,label,typology,mean_on)
data_split.lu <- rsample::initial_split(data.lu, strata = "mean_on", prop = 0.75)

bus_train.lu <- rsample::training(data_split.lu)
bus_test.lu  <- rsample::testing(data_split.lu)
names(bus_train.quarter)


cv_splits_geo.lu <- rsample::group_vfold_cv(bus_train.lu,  strata = "mean_on", group = "typology")
print(cv_splits_geo)

model_rec.lu <- recipe(mean_on ~ ., data = bus_train.lu) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))
?step_cv
model_rec.quarter

#Build the model
lm_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_engine("lm")

glmnet_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_args(penalty  = tune()) %>%
  parsnip::set_args(mixture  = tune()) %>%
  parsnip::set_engine("glmnet")

rf_plan <- parsnip::rand_forest() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 1000) %>% 
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("regression")

?set_args
XGB_plan <- parsnip::boost_tree() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 100) %>% 
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("regression")

#
glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                           mixture = seq(0,1,0.25))

rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))
xgb_grid <- expand.grid(mtry = c(3,5), 
                        min_n = c(1,5))
#Create workflow
lm_wf.lu <-
  workflows::workflow() %>% 
  add_recipe(model_rec.lu) %>% 
  add_model(lm_plan)

glmnet_wf.lu <-
  workflow() %>% 
  add_recipe(model_rec.lu) %>% 
  add_model(glmnet_plan)

rf_wf.lu <-
  workflow() %>% 
  add_recipe(model_rec.lu) %>% 
  add_model(rf_plan)
xgb_wf.lu <-
  workflow() %>% 
  add_recipe(model_rec.lu) %>% 
  add_model(XGB_plan)
# fit model to workflow and calculate metrics
control <- tune::control_resamples(save_pred = TRUE, verbose = TRUE)
library(tune)
library(yardstick)
?tune_grid
?metric_set

lm_tuned.lu <- lm_wf.lu %>%
  fit_resamples(.,
                resamples = cv_splits_geo.lu,
                control   = control,
                metrics   = metric_set(rmse, rsq))
glmnet_tuned.lu <- glmnet_wf.lu %>%
  tune_grid(.,
            resamples = cv_splits_geo.lu,
            grid      = glmnet_grid,
            control   = control,
            metrics   = metric_set(rmse, rsq))

rf_tuned.lu <- rf_wf.lu %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.lu,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned.lu <- xgb_wf.lu %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.lu,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

show_best(lm_tuned.lu, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned.lu, metric = "rmse", n = 15, maximize = FALSE)
show_best(rf_tuned.lu, metric = "rmse", n = 15, maximize = FALSE)
show_best(xgb_tuned.lu, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params.lu    <- select_best(lm_tuned.lu, metric = "rmse", maximize = FALSE)
glmnet_best_params.lu <- select_best(glmnet_tuned.lu, metric = "rmse", maximize = FALSE)
rf_best_params.lu     <- select_best(rf_tuned.lu, metric = "rmse", maximize = FALSE)
xgb_best_params.lu    <- select_best(xgb_tuned.lu, metric = "rmse", maximize = FALSE)
#Final workflow
lm_best_wf.lu     <- finalize_workflow(lm_wf.lu, lm_best_params.lu)
glmnet_best_wf.lu <- finalize_workflow(glmnet_wf.lu, glmnet_best_params.lu)
rf_best_wf.lu     <- finalize_workflow(rf_wf.lu, rf_best_params.lu)
xgb_best_wf.lu    <- finalize_workflow(xgb_wf.lu, xgb_best_params.lu)

lm_val_fit_geo.lu <- lm_best_wf.lu %>% 
  last_fit(split     = data_split.lu,
           control   = control,
           metrics   = metric_set(rmse, rsq))
glmnet_val_fit_geo.lu <- glmnet_best_wf.lu %>% 
  last_fit(split     = data_split.lu,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo.lu <- rf_best_wf.lu %>% 
  last_fit(split     = data_split.lu,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo.lu <- xgb_best_wf.lu %>% 
  last_fit(split     = data_split.lu,
           control   = control,
           metrics   = metric_set(rmse, rsq))

####################################Model Validation
# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds.lu <- collect_predictions(lm_tuned.lu) 
glmnet_best_OOF_preds.lu <- collect_predictions(glmnet_tuned.lu) %>% 
  filter(penalty  == glmnet_best_params.lu$penalty[1] & mixture == glmnet_best_params.lu$mixture[1])
rf_best_OOF_preds.lu <- collect_predictions(rf_tuned.lu) %>% 
  filter(mtry  == rf_best_params.lu$mtry[1] & min_n == rf_best_params.lu$min_n[1])

xgb_best_OOF_preds.lu <- collect_predictions(xgb_tuned.lu) %>% 
  filter(mtry  == xgb_best_params.lu$mtry[1] & min_n == xgb_best_params.lu$min_n[1])
# collect validation set predictions from last_fit model
lm_val_pred_geo.lu     <- collect_predictions(lm_val_fit_geo.lu)
glmnet_val_pred_geo.lu <- collect_predictions(glmnet_val_fit_geo.lu)
rf_val_pred_geo.lu     <- collect_predictions(rf_val_fit_geo.lu)
xgb_val_pred_geo.lu    <- collect_predictions(xgb_val_fit_geo.lu)
# Aggregate OOF predictions (they do not overlap with Validation prediction set)


OOF_preds.lu <- rbind(data.frame(dplyr::select(lm_best_OOF_preds.lu, .pred, mean_on), model = "lm"),
                           data.frame(dplyr::select(glmnet_best_OOF_preds.lu, .pred, mean_on), model = "glmnet"),
                           data.frame(dplyr::select(rf_best_OOF_preds.lu, .pred, mean_on), model = "RF"),
                           data.frame(dplyr::select(xgb_best_OOF_preds.lu, .pred, mean_on), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         #RSQUARE = yardstick::rsq(mean_on, .pred),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         #SD_RMSE = sd(yardstick::rmse_vec(mean_on, .pred)),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         #SD_MAE = sd(yardstick::mae_vec(mean_on, .pred)),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  #SD_MAPE = sd(yardstick::mape_vec(mean_on, .pred))) %>% 
  ungroup()


# Aggregate predictions from Validation set
library(tidyverse)
library(yardstick)
#lm_val_pred_geo
val_preds.lu <- rbind(data.frame(lm_val_pred_geo.lu, model = "lm"),
                           data.frame(glmnet_val_pred_geo.lu, model = "glmnet"),
                           data.frame(rf_val_pred_geo.lu, model = "rf"),
                           data.frame(xgb_val_pred_geo.lu, model = "xgb")) %>% 
  left_join(., data.lu %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(model) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()


names(final_preds.lu)
table(final_preds.lu$typology,final_preds.lu$MAPE)
summary(rf_val_pred_geo.quarter$MAE)
summary(xgb_val_pred_geo.quarter$MAE)
summary(lm_val_pred_geo.quarter$MAPE)
summary(glmnet_val_pred_geo.quarter$MAPE)
summary(rf_val_pred_geo.quarter$MAPE)
summary(xgb_val_pred_geo.quarter$MAPE)
summary(lm_val_pred_geo.quarter$RMSE)
summary(glmnet_val_pred_geo.quarter$RMSE)
summary(rf_val_pred_geo.quarter$RMSE)
summary(xgb_val_pred_geo.quarter$RMSE)
?group_by
#Rsquared
1- sum((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred) ^ 2)/sum((lm_val_pred_geo$mean_on - mean(lm_val_pred_geo$mean_on)) ^ 2)
rsq(lm_val_pred_geo.lu, mean_on, .pred)
sd(rsq(lm_val_pred_geo, mean_on, .pred))
rsq(glmnet_val_pred_geo.lu, mean_on, .pred)
rsq(rf_val_pred_geo.lu, mean_on, .pred)
rsq(xgb_val_pred_geo.lu, mean_on, .pred)
#MAE and MAPE
mean(abs(lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred))
sd(abs(lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred))
mean(abs((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)/lm_val_pred_geo$mean_on))
sd(abs((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)/lm_val_pred_geo$mean_on))
sd(abs(lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred))

mean(abs(glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred))
sd(abs(glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred))
mean(abs((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)/glmnet_val_pred_geo$mean_on))
sd(abs((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)/glmnet_val_pred_geo$mean_on))

mean(abs(rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred))
sd(abs(rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred))
mean(abs((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)/rf_val_pred_geo$mean_on))
sd(abs((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)/rf_val_pred_geo$mean_on))

mean(abs(xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred))
sd(abs(xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred))
mean(abs((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)/xgb_val_pred_geo$mean_on))
sd(abs((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)/xgb_val_pred_geo$mean_on))
#RMSE
sqrt(mean((lm_val_pred_geo.lu$mean_on - lm_val_pred_geo$.pred)^2))
sqrt(mean((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)^2))
sqrt(mean((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)^2))
sqrt(mean((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)^2))
sqrt(sd((lm_val_pred_geo$mean_on - lm_val_pred_geo$.pred)^2))
sqrt(sd((glmnet_val_pred_geo$mean_on - glmnet_val_pred_geo$.pred)^2))
sqrt(sd((rf_val_pred_geo$mean_on - rf_val_pred_geo$.pred)^2))
sqrt(sd((xgb_val_pred_geo$mean_on - xgb_val_pred_geo$.pred)^2))

yardstick::rmse_vec(lm_val_pred_geo.lu$mean_on, lm_val_pred_geo.lu$.pred)
yardstick::mape_vec(lm_val_pred_geo$mean_on, lm_val_pred_geo$.pred)
###################################Modelling part ends here, below are the visualizations##############
#MAPE chart
ggplot(data = val_preds.lu %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title= "MAPE of LandUse model on the testing set with typology")
theme_bw()
#MAE chart
ggplot(data = val_preds.lu%>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAE,1)))) +
  labs(title= "MAE of LandUse model on the testing set with typology")
theme_bw()  
#RMSE
ggplot(data = val_preds.lu %>% 
         dplyr::select(model, RMSE) %>% 
         distinct() , 
       aes(x = model, y = RMSE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(RMSE,1)))) +
  labs(title= "RMSE of LandUse model on the testing set with typology")
theme_bw() 
#Predicted vs Observed
ggplot(val_preds.lu, aes(x =.pred, y = mean_on, group = model)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  labs(title="LandUse Model: Predicted vs Observed on the testing set", subtitle= "blue line is predicted value") 
theme_bw()

#Neighborhood validation
val_MAPE_by_typology.lu <- val_preds.lu %>% 
  group_by(typology, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup() 
#Barchart of the MAE in each neighborhood
plotTheme <- function(base_size = 8) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 12,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=10),
    axis.title = element_text(size=10),
    axis.text = element_text(size=8),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic", size= 10),
    legend.text = element_text(colour = "black", face = "italic",size = 10),
    strip.text.x = element_text(size = 10)
  )
}
palette4 <- c("#eff3ff", "#bdd7e7","#6baed6","#2171b5")


as.data.frame(val_MAPE_by_typology.lu)%>%
  dplyr::select(typology, model, MAE) %>%
  #gather(Variable, MAE, -model, -typology) %>%
  ggplot(aes(typology, MAE)) + 
  geom_bar(aes(fill = model), position = "dodge", stat="identity") +
  ylim(0, 400)+
  scale_fill_manual(values = palette4) +
  facet_wrap(~typology, scale= "free", ncol=4)+
  labs(title = "LandUse Model: Mean Absolute Errors by model specification") +
  plotTheme()

as.data.frame(final_preds.lu)%>%
  dplyr::select(typology, MAPE) %>%
  #gather(Variable, MAE, -model, -typology) %>%
  ggplot(aes(typology, MAPE)) + 
  geom_bar(aes(fill = typology), position = "dodge", stat="identity") +
  #ylim(0, 400)+
  scale_fill_manual(values = palette4) +
  #facet_wrap(~typology, scale= "free", ncol=4)+
  labs(title = "LandUse Model: MAPE by model specification") +
  plotTheme()

#Amenities model
#select(c(CBD_dist, university_count, stadium_count, bar_count, school_count, residential_count, supermkt_count, 
#retailDist, officeDist,residentialDist, parkingDist,stadiumDist,trainstationDist,airportDist, mean_on))
data.ame <- data.quarter%>%
  dplyr::select(CBD_dist, university_count, stadium_count, bar_count, school_count, residential_count, supermkt_count, 
                retailDist, officeDist,residentialDist, parkingDist,stadiumDist,trainstationDist,airportDist, typology,mean_on)
data_split.ame <- rsample::initial_split(data.ame, strata = "mean_on", prop = 0.75)

bus_train.ame <- rsample::training(data_split.ame)
bus_test.ame  <- rsample::testing(data_split.ame)
names(bus_train.quarter)


cv_splits_geo.ame <- rsample::group_vfold_cv(bus_train.ame,  strata = "mean_on", group = "typology")
print(cv_splits_geo)

model_rec.ame <- recipe(mean_on ~ ., data = bus_train.ame) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))
?step_cv
model_rec.quarter

#Create workflow
lm_wf.ame <-
  workflows::workflow() %>% 
  add_recipe(model_rec.ame) %>% 
  add_model(lm_plan)

glmnet_wf.ame <-
  workflow() %>% 
  add_recipe(model_rec.ame) %>% 
  add_model(glmnet_plan)

rf_wf.ame <-
  workflow() %>% 
  add_recipe(model_rec.ame) %>% 
  add_model(rf_plan)
xgb_wf.ame <-
  workflow() %>% 
  add_recipe(model_rec.ame) %>% 
  add_model(XGB_plan)
# fit model to workflow and calculate metrics
lm_tuned.ame <- lm_wf.ame %>%
  fit_resamples(.,
                resamples = cv_splits_geo.ame,
                control   = control,
                metrics   = metric_set(rmse, rsq))
glmnet_tuned.ame <- glmnet_wf.ame %>%
  tune_grid(.,
            resamples = cv_splits_geo.ame,
            grid      = glmnet_grid,
            control   = control,
            metrics   = metric_set(rmse, rsq))

rf_tuned.ame <- rf_wf.ame %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.ame,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned.ame<- xgb_wf.ame %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.ame,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

show_best(lm_tuned.ame, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned.ame, metric = "rmse", n = 15, maximize = FALSE)
show_best(rf_tuned.ame, metric = "rmse", n = 15, maximize = FALSE)
show_best(xgb_tuned.ame, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params.ame    <- select_best(lm_tuned.ame, metric = "rmse", maximize = FALSE)
glmnet_best_params.ame <- select_best(glmnet_tuned.ame, metric = "rmse", maximize = FALSE)
rf_best_params.ame     <- select_best(rf_tuned.ame, metric = "rmse", maximize = FALSE)
xgb_best_params.ame    <- select_best(xgb_tuned.ame, metric = "rmse", maximize = FALSE)
#Final workflow
lm_best_wf.ame     <- finalize_workflow(lm_wf.ame, lm_best_params.ame)
glmnet_best_wf.ame <- finalize_workflow(glmnet_wf.ame, glmnet_best_params.ame)
rf_best_wf.ame     <- finalize_workflow(rf_wf.ame, rf_best_params.ame)
xgb_best_wf.ame    <- finalize_workflow(xgb_wf.ame, xgb_best_params.ame)

lm_val_fit_geo.ame <- lm_best_wf.ame %>% 
  last_fit(split     = data_split.ame,
           control   = control,
           metrics   = metric_set(rmse, rsq))
glmnet_val_fit_geo.ame <- glmnet_best_wf.ame %>% 
  last_fit(split     = data_split.ame,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo.ame <- rf_best_wf.ame %>% 
  last_fit(split     = data_split.ame,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo.ame <- xgb_best_wf.ame %>% 
  last_fit(split     = data_split.ame,
           control   = control,
           metrics   = metric_set(rmse, rsq))

####################################Model Validation
# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds.ame <- collect_predictions(lm_tuned.ame) 
glmnet_best_OOF_preds.ame <- collect_predictions(glmnet_tuned.ame) %>% 
  filter(penalty  == glmnet_best_params.ame$penalty[1] & mixture == glmnet_best_params.ame$mixture[1])
rf_best_OOF_preds.ame <- collect_predictions(rf_tuned.ame) %>% 
  filter(mtry  == rf_best_params.ame$mtry[1] & min_n == rf_best_params.ame$min_n[1])

xgb_best_OOF_preds.ame <- collect_predictions(xgb_tuned.ame) %>% 
  filter(mtry  == xgb_best_params.ame$mtry[1] & min_n == xgb_best_params.ame$min_n[1])
# collect validation set predictions from last_fit model
lm_val_pred_geo.ame     <- collect_predictions(lm_val_fit_geo.ame)
glmnet_val_pred_geo.ame <- collect_predictions(glmnet_val_fit_geo.ame)
rf_val_pred_geo.ame     <- collect_predictions(rf_val_fit_geo.ame)
xgb_val_pred_geo.ame    <- collect_predictions(xgb_val_fit_geo.ame)
# Aggregate OOF predictions (they do not overlap with Validation prediction set)


OOF_preds.lu <- rbind(data.frame(dplyr::select(lm_best_OOF_preds.lu, .pred, mean_on), model = "lm"),
                      data.frame(dplyr::select(glmnet_best_OOF_preds.lu, .pred, mean_on), model = "glmnet"),
                      data.frame(dplyr::select(rf_best_OOF_preds.lu, .pred, mean_on), model = "RF"),
                      data.frame(dplyr::select(xgb_best_OOF_preds.lu, .pred, mean_on), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         #RSQUARE = yardstick::rsq(mean_on, .pred),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         #SD_RMSE = sd(yardstick::rmse_vec(mean_on, .pred)),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         #SD_MAE = sd(yardstick::mae_vec(mean_on, .pred)),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  #SD_MAPE = sd(yardstick::mape_vec(mean_on, .pred))) %>% 
  ungroup()


# Aggregate predictions from Validation set
library(tidyverse)
library(yardstick)
#lm_val_pred_geo
val_preds.ame <- rbind(data.frame(lm_val_pred_geo.ame, model = "lm"),
                      data.frame(glmnet_val_pred_geo.ame, model = "glmnet"),
                      data.frame(rf_val_pred_geo.ame, model = "rf"),
                      data.frame(xgb_val_pred_geo.ame, model = "xgb")) %>% 
  left_join(., data.ame %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(model) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()



#Charts
#MAPE chart
ggplot(data = val_preds.ame %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title= "MAPE of Amenities model on the testing set with typology")
theme_bw()
#MAE chart
ggplot(data = val_preds.ame%>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAE,1)))) +
  labs(title= "MAE of Amenities model on the testing set with typology")
theme_bw()  
#RMSE
ggplot(data = val_preds.ame %>% 
         dplyr::select(model, RMSE) %>% 
         distinct() , 
       aes(x = model, y = RMSE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(RMSE,1)))) +
  labs(title= "RMSE of Amenities model on the testing set with typology")
theme_bw() 

rsq(lm_val_pred_geo.ame, mean_on, .pred)
sd(rsq(lm_val_pred_geo, mean_on, .pred))
rsq(glmnet_val_pred_geo.ame, mean_on, .pred)
rsq(rf_val_pred_geo.ame, mean_on, .pred)
rsq(xgb_val_pred_geo.ame, mean_on, .pred)
#Neighborhood validation
val_MAPE_by_typology.ame <- val_preds.ame %>% 
  group_by(typology, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup() 
#Barchart of the MAE in each neighborhood

as.data.frame(val_MAPE_by_typology.ame)%>%
  dplyr::select(typology, model, MAE) %>%
  #gather(Variable, MAE, -model, -typology) %>%
  ggplot(aes(typology, MAE)) + 
  geom_bar(aes(fill = model), position = "dodge", stat="identity") +
  ylim(0, 400)+
  scale_fill_manual(values = palette4) +
  facet_wrap(~typology, scale= "free", ncol=4)+
  labs(title = "Amenities Model: Mean Absolute Errors by model specification") +
  plotTheme()

#
#RouteType model
#select(c(nshifts, InOut, SouthNorth, Crosstown, Feeder, Flyer, Night.Owl, hotline_1, mean_on))
data.route <- data.quarter%>%
  dplyr::select(nshifts, InOut, SouthNorth, Crosstown, Feeder, Flyer, Night.Owl, hotline_1, typology,mean_on)
data_split.route <- rsample::initial_split(data.route, strata = "mean_on", prop = 0.75)

bus_train.route <- rsample::training(data_split.route)
bus_test.route  <- rsample::testing(data_split.route)
names(bus_train.quarter)


cv_splits_geo.route <- rsample::group_vfold_cv(bus_train.route,  strata = "mean_on", group = "typology")
print(cv_splits_geo)

model_rec.route <- recipe(mean_on ~ ., data = bus_train.route) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))


#Create workflow
lm_wf.route <-
  workflows::workflow() %>% 
  add_recipe(model_rec.route) %>% 
  add_model(lm_plan)

glmnet_wf.route <-
  workflow() %>% 
  add_recipe(model_rec.route) %>% 
  add_model(glmnet_plan)

rf_wf.route <-
  workflow() %>% 
  add_recipe(model_rec.route) %>% 
  add_model(rf_plan)
xgb_wf.route <-
  workflow() %>% 
  add_recipe(model_rec.route) %>% 
  add_model(XGB_plan)
# fit model to workflow and calculate metrics
lm_tuned.route <- lm_wf.route %>%
  fit_resamples(.,
                resamples = cv_splits_geo.route,
                control   = control,
                metrics   = metric_set(rmse, rsq))
glmnet_tuned.route <- glmnet_wf.route %>%
  tune_grid(.,
            resamples = cv_splits_geo.route,
            grid      = glmnet_grid,
            control   = control,
            metrics   = metric_set(rmse, rsq))

rf_tuned.route <- rf_wf.route %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.route,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned.route<- xgb_wf.route %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.route,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

show_best(lm_tuned.route, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned.route, metric = "rmse", n = 15, maximize = FALSE)
show_best(rf_tuned.route, metric = "rmse", n = 15, maximize = FALSE)
show_best(xgb_tuned.route, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params.route    <- select_best(lm_tuned.route, metric = "rmse", maximize = FALSE)
glmnet_best_params.route <- select_best(glmnet_tuned.route, metric = "rmse", maximize = FALSE)
rf_best_params.route     <- select_best(rf_tuned.route, metric = "rmse", maximize = FALSE)
xgb_best_params.route    <- select_best(xgb_tuned.route, metric = "rmse", maximize = FALSE)
#Final workflow
lm_best_wf.route     <- finalize_workflow(lm_wf.route, lm_best_params.route)
glmnet_best_wf.route <- finalize_workflow(glmnet_wf.route, glmnet_best_params.route)
rf_best_wf.route     <- finalize_workflow(rf_wf.route, rf_best_params.route)
xgb_best_wf.route    <- finalize_workflow(xgb_wf.route, xgb_best_params.route)

lm_val_fit_geo.route <- lm_best_wf.route %>% 
  last_fit(split     = data_split.route,
           control   = control,
           metrics   = metric_set(rmse, rsq))
glmnet_val_fit_geo.route <- glmnet_best_wf.route %>% 
  last_fit(split     = data_split.route,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo.route <- rf_best_wf.route %>% 
  last_fit(split     = data_split.route,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo.route <- xgb_best_wf.route %>% 
  last_fit(split     = data_split.route,
           control   = control,
           metrics   = metric_set(rmse, rsq))

####################################Model Validation
# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds.route <- collect_predictions(lm_tuned.route) 
glmnet_best_OOF_preds.route <- collect_predictions(glmnet_tuned.route) %>% 
  filter(penalty  == glmnet_best_params.route$penalty[1] & mixture == glmnet_best_params.route$mixture[1])
rf_best_OOF_preds.route <- collect_predictions(rf_tuned.route) %>% 
  filter(mtry  == rf_best_params.route$mtry[1] & min_n == rf_best_params.route$min_n[1])

xgb_best_OOF_preds.route <- collect_predictions(xgb_tuned.route) %>% 
  filter(mtry  == xgb_best_params.route$mtry[1] & min_n == xgb_best_params.route$min_n[1])
# collect validation set predictions from last_fit model
lm_val_pred_geo.route     <- collect_predictions(lm_val_fit_geo.route)
glmnet_val_pred_geo.route <- collect_predictions(glmnet_val_fit_geo.route)
rf_val_pred_geo.route     <- collect_predictions(rf_val_fit_geo.route)
xgb_val_pred_geo.route    <- collect_predictions(xgb_val_fit_geo.route)

# Aggregate predictions from Validation set
library(tidyverse)
library(yardstick)
#lm_val_pred_geo
val_preds.route <- rbind(data.frame(lm_val_pred_geo.route, model = "lm"),
                       data.frame(glmnet_val_pred_geo.route, model = "glmnet"),
                       data.frame(rf_val_pred_geo.route, model = "rf"),
                       data.frame(xgb_val_pred_geo.route, model = "xgb")) %>% 
  left_join(., data.route %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(model) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()


#Charts
#MAPE chart
ggplot(data = val_preds.route %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title= "MAPE of Route model on the testing set with typology")
theme_bw()
#MAE chart
ggplot(data = val_preds.route%>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAE,1)))) +
  labs(title= "MAE of Route model on the testing set with typology")
theme_bw()  
#RMSE
ggplot(data = val_preds.route %>% 
         dplyr::select(model, RMSE) %>% 
         distinct() , 
       aes(x = model, y = RMSE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(RMSE,1)))) +
  labs(title= "RMSE of LandUse model on the testing set with typology")
theme_bw() 

rsq(lm_val_pred_geo.route, mean_on, .pred)
sd(rsq(lm_val_pred_geo, mean_on, .pred))
rsq(glmnet_val_pred_geo.route, mean_on, .pred)
rsq(rf_val_pred_geo.route, mean_on, .pred)
rsq(xgb_val_pred_geo.route, mean_on, .pred)
#Neighborhood validation
val_MAPE_by_typology.route <- val_preds.route %>% 
  group_by(typology, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup() 
#Barchart of the MAE in each neighborhood

as.data.frame(val_MAPE_by_typology.route)%>%
  dplyr::select(typology, model, MAE) %>%
  #gather(Variable, MAE, -model, -typology) %>%
  ggplot(aes(typology, MAE)) + 
  geom_bar(aes(fill = model), position = "dodge", stat="identity") +
  ylim(0, 400)+
  scale_fill_manual(values = palette4) +
  facet_wrap(~typology, scale= "free", ncol=4)+
  labs(title = "Route Model: Mean Absolute Errors by model specification") +
  plotTheme()

##Demographics model
#select(c(nshifts, InOut, SouthNorth, Crosstown, Feeder, Flyer, Night.Owl, hotline_1, mean_on))
names(data.demo)
sum(is.na(data.demo$medinc))
sum(is.na(medInc$estimate))

write.csv(data.quarter, "C:/Upenn/Practicum/Data/quarter.csv")
names(data.quarter)

sum(is.na(data.quarter))
data.quarter2 <- data.quarter %>%
  drop_na()

data.demo <- data.quarter2%>%
  dplyr::select(medInc,NoVeh, OneVeh, TwoVeh, FiveVeh, typology,mean_on)

data_split.demo <- rsample::initial_split(data.demo, strata = "mean_on", prop = 0.75)

bus_train.demo <- rsample::training(data_split.demo)
bus_test.demo  <- rsample::testing(data_split.demo)
names(bus_train.quarter)


cv_splits_geo.demo <- rsample::group_vfold_cv(bus_train.demo,  strata = "mean_on", group = "typology")
print(cv_splits_geo)

model_rec.demo <- recipe(mean_on ~ ., data = bus_train.demo) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))


#Create workflow
lm_wf.demo <-
  workflows::workflow() %>% 
  add_recipe(model_rec.demo) %>% 
  add_model(lm_plan)

glmnet_wf.demo <-
  workflow() %>% 
  add_recipe(model_rec.demo) %>% 
  add_model(glmnet_plan)

rf_wf.demo <-
  workflow() %>% 
  add_recipe(model_rec.demo) %>% 
  add_model(rf_plan)
xgb_wf.demo <-
  workflow() %>% 
  add_recipe(model_rec.demo) %>% 
  add_model(XGB_plan)
# fit model to workflow and calculate metrics
lm_tuned.demo <- lm_wf.demo %>%
  fit_resamples(.,
                resamples = cv_splits_geo.demo,
                control   = control,
                metrics   = metric_set(rmse, rsq))
glmnet_tuned.demo <- glmnet_wf.demo %>%
  tune_grid(.,
            resamples = cv_splits_geo.demo,
            grid      = glmnet_grid,
            control   = control,
            metrics   = metric_set(rmse, rsq))

rf_tuned.demo <- rf_wf.demo %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.demo,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned.demo<- xgb_wf.demo %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.demo,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

show_best(lm_tuned.demo, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned.demo, metric = "rmse", n = 15, maximize = FALSE)
show_best(rf_tuned.demo, metric = "rmse", n = 15, maximize = FALSE)
show_best(xgb_tuned.demo, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params.demo    <- select_best(lm_tuned.demo, metric = "rmse", maximize = FALSE)
glmnet_best_params.demo <- select_best(glmnet_tuned.demo, metric = "rmse", maximize = FALSE)
rf_best_params.demo     <- select_best(rf_tuned.demo, metric = "rmse", maximize = FALSE)
xgb_best_params.demo    <- select_best(xgb_tuned.demo, metric = "rmse", maximize = FALSE)
#Final workflow
lm_best_wf.demo     <- finalize_workflow(lm_wf.demo, lm_best_params.demo)
glmnet_best_wf.demo <- finalize_workflow(glmnet_wf.demo, glmnet_best_params.demo)
rf_best_wf.demo     <- finalize_workflow(rf_wf.demo, rf_best_params.demo)
xgb_best_wf.demo    <- finalize_workflow(xgb_wf.demo, xgb_best_params.demo)

lm_val_fit_geo.demo <- lm_best_wf.demo %>% 
  last_fit(split     = data_split.demo,
           control   = control,
           metrics   = metric_set(rmse, rsq))
glmnet_val_fit_geo.demo <- glmnet_best_wf.demo %>% 
  last_fit(split     = data_split.demo,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo.demo <- rf_best_wf.demo %>% 
  last_fit(split     = data_split.demo,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo.demo <- xgb_best_wf.demo %>% 
  last_fit(split     = data_split.demo,
           control   = control,
           metrics   = metric_set(rmse, rsq))

####################################Model Validation
# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds.demo <- collect_predictions(lm_tuned.demo) 
glmnet_best_OOF_preds.demo <- collect_predictions(glmnet_tuned.demo) %>% 
  filter(penalty  == glmnet_best_params.demo$penalty[1] & mixture == glmnet_best_params.demo$mixture[1])
rf_best_OOF_preds.demo <- collect_predictions(rf_tuned.demo) %>% 
  filter(mtry  == rf_best_params.demo$mtry[1] & min_n == rf_best_params.demo$min_n[1])

xgb_best_OOF_preds.demo <- collect_predictions(xgb_tuned.demo) %>% 
  filter(mtry  == xgb_best_params.demo$mtry[1] & min_n == xgb_best_params.demo$min_n[1])
# collect validation set predictions from last_fit model
lm_val_pred_geo.demo     <- collect_predictions(lm_val_fit_geo.demo)
glmnet_val_pred_geo.demo <- collect_predictions(glmnet_val_fit_geo.demo)
rf_val_pred_geo.demo     <- collect_predictions(rf_val_fit_geo.demo)
xgb_val_pred_geo.demo    <- collect_predictions(xgb_val_fit_geo.demo)

# Aggregate predictions from Validation set
library(tidyverse)
library(yardstick)
#lm_val_pred_geo
val_preds.demo <- rbind(data.frame(lm_val_pred_geo.demo, model = "lm"),
                         data.frame(glmnet_val_pred_geo.demo, model = "glmnet"),
                         data.frame(rf_val_pred_geo.demo, model = "rf"),
                         data.frame(xgb_val_pred_geo.demo, model = "xgb")) %>% 
  left_join(., data.demo %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(model) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()




#Charts
#MAPE chart
ggplot(data = val_preds.demo %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title= "MAPE of Demographics model on the testing set")
theme_bw()
#MAE chart
ggplot(data = val_preds.demo%>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAE,1)))) +
  labs(title= "MAE of Demographics model on the testing set")
theme_bw()  
#RMSE
ggplot(data = val_preds.demo %>% 
         dplyr::select(model, RMSE) %>% 
         distinct() , 
       aes(x = model, y = RMSE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(RMSE,1)))) +
  labs(title= "RMSE of LandUse model on the testing set with typology")
theme_bw() 

rsq(lm_val_pred_geo.demo, mean_on, .pred)
sd(rsq(lm_val_pred_geo, mean_on, .pred))
rsq(glmnet_val_pred_geo.demo, mean_on, .pred)
rsq(rf_val_pred_geo.demo, mean_on, .pred)
rsq(xgb_val_pred_geo.demo, mean_on, .pred)

#Neighborhood validation
val_MAPE_by_typology.demo <- val_preds.demo %>% 
  group_by(typology, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup() 
#Barchart of the MAE in each neighborhood

as.data.frame(val_MAPE_by_typology.demo)%>%
  dplyr::select(typology, model, MAE) %>%
  #gather(Variable, MAE, -model, -typology) %>%
  ggplot(aes(typology, MAE)) + 
  geom_bar(aes(fill = model), position = "dodge", stat="identity") +
  ylim(0, 400)+
  scale_fill_manual(values = palette4) +
  facet_wrap(~typology, scale= "free", ncol=4)+
  labs(title = "Demographics Model: Mean Absolute Errors by model specification") +
  plotTheme()

#Select the best ensemble models and build up the final dataset for the next step
###Choosing XGB model for lu model
final_xgb_best_OOF_preds.lu <- xgb_best_OOF_preds.lu %>%
  dplyr::select(.pred,mean_on,.row)
final_xgb_val_pred_geo.lu <- xgb_val_pred_geo.lu %>%
  dplyr::select(.pred,mean_on,.row)
final_preds.lu <- rbind(data.frame(final_xgb_best_OOF_preds.lu),data.frame(final_xgb_val_pred_geo.lu))%>%
  left_join(., data.lu %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(typology) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()


#Choose rf model for amenities
final_rf_best_OOF_preds.ame <- rf_best_OOF_preds.ame %>%
  dplyr::select(.pred,mean_on,.row)
final_rf_val_pred_geo.ame <- rf_val_pred_geo.ame %>%
  dplyr::select(.pred,mean_on,.row)
final_preds.ame <- rbind(data.frame(final_rf_best_OOF_preds.ame),data.frame(final_rf_val_pred_geo.ame))%>%
  left_join(., data.ame %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(typology) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()
names(final_preds.ame)
table(final_preds.ame$typology,final_preds.ame$MAPE)

#Choose rf model for routes
final_rf_best_OOF_preds.route <- rf_best_OOF_preds.route %>%
  dplyr::select(.pred,mean_on,.row)
final_rf_val_pred_geo.route <- rf_val_pred_geo.route %>%
  dplyr::select(.pred,mean_on,.row)
final_preds.route <- rbind(data.frame(final_rf_best_OOF_preds.route),data.frame(final_rf_val_pred_geo.route))%>%
  left_join(., data.route %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(typology) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()
names(final_preds.route)
table(final_preds.ame$typology,final_preds.ame$MAPE)

#Choose rf model for demographics
final_rf_best_OOF_preds.demo <- rf_best_OOF_preds.demo %>%
  dplyr::select(.pred,mean_on,.row)
final_rf_val_pred_geo.demo <- rf_val_pred_geo.demo %>%
  dplyr::select(.pred,mean_on,.row)
final_preds.demo <- rbind(data.frame(final_rf_best_OOF_preds.demo),data.frame(final_rf_val_pred_geo.demo))%>%
  left_join(., data.demo %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(typology) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()
names(final_preds.ame)
table(final_preds.ame$typology,final_preds.ame$MAPE)

#now, join all the stories's predictions together
final_preds.ame2 <- final_preds.ame %>%
  dplyr::select(mean_on, .pred)
names(final_preds.ame2)[2]<- ".pred_ame"
final_preds <- plyr::join(final_preds.lu, final_preds.ame2)

sum(is.na(final_preds))
final_preds.route2 <- final_preds.route %>%
  dplyr::select(mean_on, .pred)
names(final_preds.route2)[2]<- ".pred_route"
final_preds <- plyr::join(final_preds, final_preds.route2)

final_preds.demo2 <- final_preds.demo %>%
  dplyr::select(mean_on, .pred)
sum(is.na(final_preds.demo2$.pred_demo))
names(final_preds.demo2)[2]<- ".pred_demo"
final_preds.1 <- plyr::join(final_preds, final_preds.demo2,type="left")

sum(is.na(final_preds.1))

names(final_preds)[1]<- ".pred_lu"
final_preds <- final_preds %>%
  dplyr::select(mean_on, .pred_ame, .pred_demo, .pred_route, .pred_lu, typology)
names(final_preds)
#Correlation Matrix
final_preds2 <- final_preds %>%
  dplyr::select(mean_on, .pred_ame, .pred_demo, .pred_route, .pred_lu)%>%
  drop_na()

M <- cor(final_preds2)
library(corrplot)
corrplot(M, method = "number")

#Correlation Plots
correlation <- final_preds2 %>%
  #st_set_geometry(all_x3, NULL) %>%
  #select(-c(STOP_ID, label, TRUSTEE)) %>%
  gather(Variable, Value, -mean_on)

correlation.cor <-
  correlation %>%
  group_by(Variable) %>%
  summarize(correlation = cor(as.numeric(Value), mean_on, use = "complete.obs"))

ggplot(correlation, aes(Value, mean_on)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "#2C7BB6") +
  facet_wrap(~Variable, ncol = 5, scales = "free") +
  labs(title = "Mean Ridership as A Function of Potential Factors")


#Try model ensemble for four stories
#Add spatial lags
names(data.quarter)
#spatial_lag <- data.quarter %>%
#  dplyr::select(mean_on, spatial_lag2, spatial_lag3, spatial_lag5)
#final_preds3 <- plyr::join(final_preds, spatial_lag)
final_preds3 <- final_preds %>%
  drop_na()
#Now modeling begins
data_split.ens <- rsample::initial_split(final_preds3, strata = "mean_on", prop = 0.75)

bus_train.ens <- rsample::training(data_split.ens)
bus_test.ens  <- rsample::testing(data_split.ens)
names(bus_train.quarter)


cv_splits_geo.ens <- rsample::group_vfold_cv(bus_train.ens,  strata = "mean_on", group = "typology")
print(cv_splits_geo)

model_rec.ens <- recipe(mean_on ~ ., data = bus_train.ens) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))

rf_plan <- parsnip::rand_forest() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 1000) %>% 
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("regression")

?set_args
XGB_plan <- parsnip::boost_tree() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 100) %>% 
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("regression")


#Create workflow
lm_wf.ens <-
  workflows::workflow() %>% 
  add_recipe(model_rec.ens) %>% 
  add_model(lm_plan)

glmnet_wf.ens <-
  workflow() %>% 
  add_recipe(model_rec.ens) %>% 
  add_model(glmnet_plan)

rf_wf.ens <-
  workflow() %>% 
  add_recipe(model_rec.ens) %>% 
  add_model(rf_plan)
xgb_wf.ens <-
  workflow() %>% 
  add_recipe(model_rec.ens) %>% 
  add_model(XGB_plan)

rf_grid2 <- expand.grid(mtry = c(2,3), 
                       min_n = c(1,3))

xgb_grid2 <- expand.grid(mtry = c(3,3), 
                        min_n = c(1,3))
# fit model to workflow and calculate metrics
lm_tuned.ens <- lm_wf.ens %>%
  fit_resamples(.,
                resamples = cv_splits_geo.ens,
                control   = control,
                metrics   = metric_set(rmse, rsq))
glmnet_tuned.ens <- glmnet_wf.ens %>%
  tune_grid(.,
            resamples = cv_splits_geo.ens,
            grid      = glmnet_grid,
            control   = control,
            metrics   = metric_set(rmse, rsq))

rf_tuned.ens <- rf_wf.ens %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.ens,
                  grid      = rf_grid2,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned.ens<- xgb_wf.ens %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.ens,
                  grid      = xgb_grid2,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

show_best(lm_tuned.ens, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned.ens, metric = "rmse", n = 15, maximize = FALSE)
show_best(rf_tuned.ens, metric = "rmse", n = 15, maximize = FALSE)
show_best(xgb_tuned.ens, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params.ens    <- select_best(lm_tuned.ens, metric = "rmse", maximize = FALSE)
glmnet_best_params.ens <- select_best(glmnet_tuned.ens, metric = "rmse", maximize = FALSE)
rf_best_params.ens     <- select_best(rf_tuned.ens, metric = "rmse", maximize = FALSE)
xgb_best_params.ens    <- select_best(xgb_tuned.ens, metric = "rmse", maximize = FALSE)
#Final workflow
lm_best_wf.ens     <- finalize_workflow(lm_wf.ens, lm_best_params.ens)
glmnet_best_wf.ens <- finalize_workflow(glmnet_wf.ens, glmnet_best_params.ens)
rf_best_wf.ens     <- finalize_workflow(rf_wf.ens, rf_best_params.ens)
xgb_best_wf.ens    <- finalize_workflow(xgb_wf.ens, xgb_best_params.ens)

lm_val_fit_geo.ens <- lm_best_wf.ens %>% 
  last_fit(split     = data_split.ens,
           control   = control,
           metrics   = metric_set(rmse, rsq))
glmnet_val_fit_geo.ens <- glmnet_best_wf.ens %>% 
  last_fit(split     = data_split.ens,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo.ens <- rf_best_wf.ens %>% 
  last_fit(split     = data_split.ens,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo.ens <- xgb_best_wf.ens %>% 
  last_fit(split     = data_split.ens,
           control   = control,
           metrics   = metric_set(rmse, rsq))

####################################Model Validation
# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds.ens <- collect_predictions(lm_tuned.ens) 
glmnet_best_OOF_preds.ens <- collect_predictions(glmnet_tuned.ens) %>% 
  filter(penalty  == glmnet_best_params.ens$penalty[1] & mixture == glmnet_best_params.ens$mixture[1])
rf_best_OOF_preds.ens <- collect_predictions(rf_tuned.ens) %>% 
  filter(mtry  == rf_best_params.ens$mtry[1] & min_n == rf_best_params.ens$min_n[1])

xgb_best_OOF_preds.ens <- collect_predictions(xgb_tuned.ens) %>% 
  filter(mtry  == xgb_best_params.ens$mtry[1] & min_n == xgb_best_params.ens$min_n[1])
# collect validation set predictions from last_fit model
lm_val_pred_geo.ens     <- collect_predictions(lm_val_fit_geo.ens)
glmnet_val_pred_geo.ens <- collect_predictions(glmnet_val_fit_geo.ens)
rf_val_pred_geo.ens     <- collect_predictions(rf_val_fit_geo.ens)
xgb_val_pred_geo.ens    <- collect_predictions(xgb_val_fit_geo.ens)

# Aggregate predictions from Validation set
library(tidyverse)
library(yardstick)
#lm_val_pred_geo
val_preds.ens <- rbind(data.frame(lm_val_pred_geo.ens, model = "lm"),
                         data.frame(glmnet_val_pred_geo.ens, model = "glmnet"),
                         data.frame(rf_val_pred_geo.ens, model = "rf"),
                         data.frame(xgb_val_pred_geo.ens, model = "xgb")) %>% 
  left_join(., final_preds3 %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(model) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()


#Charts
#MAPE chart
ggplot(data = val_preds.ens %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title= "MAPE of Ensemble model on the testing set with typology")
theme_bw()
#MAE chart
ggplot(data = val_preds.ens%>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAE,1)))) +
  labs(title= "MAE of Ensemble model on the testing set with typology")
theme_bw()  
#RMSE
ggplot(data = val_preds.route %>% 
         dplyr::select(model, RMSE) %>% 
         distinct() , 
       aes(x = model, y = RMSE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(RMSE,1)))) +
  labs(title= "RMSE of LandUse model on the testing set with typology")
theme_bw() 

rsq(lm_val_pred_geo.route, mean_on, .pred)
sd(rsq(lm_val_pred_geo, mean_on, .pred))
rsq(glmnet_val_pred_geo.route, mean_on, .pred)
rsq(rf_val_pred_geo.ens, mean_on, .pred)
rsq(xgb_val_pred_geo.route, mean_on, .pred)
#Neighborhood validation
val_MAPE_by_typology.route <- val_preds.route %>% 
  group_by(typology, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup() 
#Barchart of the MAE in each neighborhood

as.data.frame(val_MAPE_by_typology.route)%>%
  dplyr::select(typology, model, MAE) %>%
  #gather(Variable, MAE, -model, -typology) %>%
  ggplot(aes(typology, MAE)) + 
  geom_bar(aes(fill = model), position = "dodge", stat="identity") +
  ylim(0, 400)+
  scale_fill_manual(values = palette4) +
  facet_wrap(~typology, scale= "free", ncol=4)+
  labs(title = "Route Model: Mean Absolute Errors by model specification") +
  plotTheme()

###################Test on Scenarios
Scen1 <- Scen1%>%
  drop_na()
Scen1 <- plyr::join(Scen1, typology)

Scen1 <- Scen1 %>%
  dplyr::select(building_area,commercial,residential,civic,transportation,industrial,label,typology,mean_on)

data_split.scen1 <- rsample::initial_split(Scen1, strata = "mean_on", prop = 0.75)
model_rec.scen1 <- recipe(mean_on ~ ., data = Scen1) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))
xgb_wf.scen1.lu <-
  workflow() %>% 
  add_recipe(model_rec.scen1) %>% 
  add_model(XGB_plan)

xgb_best_wf.scen1.lu    <- finalize_workflow(xgb_wf.scen1.lu, xgb_best_params.lu)
xgb_val_fit_geo.scen1.lu<- xgb_best_wf.scen1.lu %>% 
  last_fit(split     = data_split.scen1,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_pred_geo.scen1.lu    <- collect_predictions(xgb_val_fit_geo.scen1.lu)

final_xgb_val_pred_geo.scen1.lu <- xgb_val_pred_geo.scen1.lu %>%
  dplyr::select(.pred,mean_on,.row)
final_preds.scen1.lu <- rbind(data.frame(final_xgb_best_OOF_preds.lu),data.frame(final_xgb_val_pred_geo.scen1.lu))%>%
  left_join(., Scen1 %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(typology) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()


scen1_prediction <- final_preds.scen1.lu %>%
  dplyr::select(.pred, mean_on)

names(my_data)[1] <- "sepal_length"
names(scen1_prediction)[1]<- "new"
test <- plyr::join(scen1_prediction,final_preds.lu, type="left")
sum(test$new)
sum(test$.pred)

#Test on the ensemble model
sum(is.na(final_preds$.pred_demo))
final_preds.scen1 <- plyr::join(scen1_prediction, final_preds)
names(final_preds.scen1)
final_preds.scen1$.pred_lu<- NULL
sum(is.na())
sum(is.na(final_preds.scen1$.pred_demo))
data_split.final.scen1 <- rsample::initial_split(final_preds.scen1, strata = "mean_on", prop = 0.75)
model_rec.final.scen1 <- recipe(mean_on ~ ., data = final_preds.scen1) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))
rf_wf.scen1.ens <-
  workflow() %>% 
  add_recipe(model_rec.final.scen1) %>% 
  add_model(rf_plan)

rf_best_wf.scen1.ens    <- finalize_workflow(rf_wf.scen1.ens, rf_best_params.ens)
rf_val_fit_geo.scen1.ens<- rf_best_wf.scen1.ens %>% 
  last_fit(split     = data_split.final.scen1,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_pred_geo.scen1.ens    <- collect_predictions(rf_val_fit_geo.scen1.ens)

final_rf_val_pred_geo.scen1.ens <- rf_val_pred_geo.scen1.ens %>%
  dplyr::select(.pred,mean_on,.row)

final_preds.scen1.ens <- rbind(data.frame(rf_best_OOF_preds.ens),data.frame(final_rf_val_pred_geo.scen1.ens))%>%
  left_join(., final_preds.scen1 %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(typology) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()

####################Create the kitchen sink model with selected variables using random forest
sum(is.na(sce0))
library(dplyr)
install.packages("mltools")
library(mltools)
sce0<-sce %>% drop_na()

sce0 <- plyr::join(sce0, typology, type= "left")

library(data.table)
sce0 <- 
  as.data.table(sce0)%>%
  one_hot(cols = "SN_cat",
          dropCols = TRUE)
names(sce0)
sce0 <- 
  as.data.table(sce0)%>%
  one_hot(cols = "Crosstown_cat",
          dropCols = TRUE)

sce0 <- 
  as.data.table(sce0)%>%
  one_hot(cols = "Express_cat",
          dropCols = TRUE)

sce0 <- 
  as.data.table(sce0)%>%
  one_hot(cols = "Local_cat",
          dropCols = TRUE)
sce0 <- 
  as.data.table(sce0)%>%
  one_hot(cols = "Flyer_cat",
          dropCols = TRUE)
sce0 <- 
  as.data.table(sce0)%>%
  one_hot(cols = "NightOwl_cat",
          dropCols = TRUE)
sce0 <- 
  as.data.table(sce0)%>%
  one_hot(cols = "HighFreq_cat",
          dropCols = TRUE)
sce0 <- 
  as.data.table(sce0)%>%
  one_hot(cols = "InOut_cat",
          dropCols = TRUE)
sce0 <- 
  as.data.table(sce0)%>%
  one_hot(cols = "Clockwise_cat",
          dropCols = TRUE)
sce0 <- 
  as.data.table(sce0)%>%
  one_hot(cols = "utshuttle_cat",
          dropCols = TRUE)
sce0 <- 
  as.data.table(sce0)%>%
  one_hot(cols = "Special_cat",
          dropCols = TRUE)

sce0 <- sce0 %>%dplyr::select(-STOP_ID)
data_split.sce0 <- rsample::initial_split(sce0, strata = "mean_on", prop = 0.75)

bus_train.sce0 <- rsample::training(data_split.sce0)
bus_test.sce0  <- rsample::testing(data_split.sce0)
names(bus_train.quarter)

cv_splits_geo.sce0 <- rsample::group_vfold_cv(bus_train.sce0,  strata = "mean_on", group = "typology")
print(cv_splits_geo)

model_rec.sce0 <- recipe(mean_on ~ ., data = bus_train.sce0) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))


#Create workflow

rf_wf.sce0 <-
  workflow() %>% 
  add_recipe(model_rec.sce0) %>% 
  add_model(rf_plan)

# fit model to workflow and calculate metrics
#Metrics are changes from rmse + rsq to only rsq
rf_tuned.sce0 <- rf_wf.sce0 %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo.sce0,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rsq))

?tune_grid
show_best(rf_tuned.sce0, metric = "rsq", n = 15, maximize = FALSE)



rf_best_params.sce0     <- select_best(rf_tuned.sce0, metric = "rsq", maximize = FALSE)

#Final workflow
rf_best_wf.sce0     <- finalize_workflow(rf_wf.sce0, rf_best_params.sce0)

rf_val_fit_geo.sce0 <- rf_best_wf.sce0 %>% 
  last_fit(split     = data_split.sce0,
           control   = control,
           metrics   = metric_set(rsq))

####################################Model Validation
# Pull best hyperparam preds from out-of-fold predictions
rf_best_OOF_preds.sce0 <- collect_predictions(rf_tuned.sce0) %>% 
  filter(mtry  == rf_best_params.sce0$mtry[1] & min_n == rf_best_params.sce0$min_n[1])
# collect validation set predictions from last_fit model
rf_val_pred_geo.sce0     <- collect_predictions(rf_val_fit_geo.sce0)
# Aggregate predictions from Validation set
library(tidyverse)
library(yardstick)
#lm_val_pred_geo
rf_best_OOF_preds.sce0 <- rf_best_OOF_preds.sce0 %>% dplyr::select(-min_n, -mtry)
val_preds.sce0 <- rbind(data.frame(rf_val_pred_geo.sce0), data.frame(rf_best_OOF_preds.sce0) )%>% 
  left_join(., sce0 %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(typology) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()

val_MAPE_by_typology.sce0 <- val_preds.sce0 %>% 
  group_by(typology) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup() 
#Barchart of the MAE in each neighborhood
plotTheme <- function(base_size = 10) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 20,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text = element_text(size=15),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic", size= 20),
    legend.text = element_text(colour = "black", face = "italic",size = 20),
    strip.text.x = element_text(size = 15)
  )
}
palette4 <- c("#eff3ff", "#bdd7e7","#6baed6","#2171b5")


as.data.frame(val_MAPE_by_typology.sce0)%>%
  dplyr::select(typology,MAPE) %>%
  #gather(Variable, MAE, -model, -typology) %>%
  ggplot(aes(typology, MAPE)) + 
  geom_bar(aes(fill = typology), position = "dodge", stat="identity") +
  ylim(0, 120)+
  scale_fill_manual(values = palette4) +
  facet_wrap(~typology, scale= "free", ncol=4)+
  labs(title = "MAPE of the random forest model") +
  plotTheme()

rsq(rf_val_pred_geo.sce0, mean_on, .pred)
#Test on the first scenario: adding building area at West Campus
data.west <- data.west %>% drop_na()
data.west<- plyr::join(data.west, typology, type = "left")
#One hot encoding
data.west<- 
  as.data.table(data.west)%>%
  one_hot(cols = "SN_cat",
          dropCols = TRUE)

data.west <- 
  as.data.table(data.west)%>%
  one_hot(cols = "Crosstown_cat",
          dropCols = TRUE)

data.west <- 
  as.data.table(data.west)%>%
  one_hot(cols = "Express_cat",
          dropCols = TRUE)

data.west <- 
  as.data.table(data.west)%>%
  one_hot(cols = "Local_cat",
          dropCols = TRUE)
data.west <- 
  as.data.table(data.west)%>%
  one_hot(cols = "Flyer_cat",
          dropCols = TRUE)
data.west <- 
  as.data.table(data.west)%>%
  one_hot(cols = "NightOwl_cat",
          dropCols = TRUE)
data.west <- 
  as.data.table(data.west)%>%
  one_hot(cols = "HighFreq_cat",
          dropCols = TRUE)
data.west<- 
  as.data.table(data.west)%>%
  one_hot(cols = "InOut_cat",
          dropCols = TRUE)
data.west <- 
  as.data.table(data.west)%>%
  one_hot(cols = "Clockwise_cat",
          dropCols = TRUE)
data.west <- 
  as.data.table(data.west)%>%
  one_hot(cols = "utshuttle_cat",
          dropCols = TRUE)
data.west <- 
  as.data.table(data.west)%>%
  one_hot(cols = "Special_cat",
          dropCols = TRUE)
data.west <- data.west %>%dplyr::select(-STOP_ID)
data_split.west <- rsample::initial_split(data.west, strata = "mean_on", prop = 0.75)

model_rec.west <- recipe(mean_on ~ ., data = data.west) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))
rf_wf.west <-
  workflow() %>% 
  add_recipe(model_rec.west) %>% 
  add_model(rf_plan)

rf_best_wf.west    <- finalize_workflow(rf_wf.west, rf_best_params.sce0)

rf_val_fit_geo.west<- rf_best_wf.west %>% 
  last_fit(split     = data_split.west,
           control   = control,
           metrics   = metric_set(rsq))
?last_fit
?collect_predictions
rf_val_pred_geo.west    <- collect_predictions(rf_val_fit_geo.west)

final_rf_val_pred_geo.west <- rf_val_pred_geo.west %>%
  dplyr::select(.pred,mean_on,.row)

rf_best_OOF_preds.sce0.1 <- rf_best_OOF_preds.sce0%>% dplyr::select(-id)
final_preds.west <- rbind(data.frame(final_rf_val_pred_geo.west),data.frame(rf_best_OOF_preds.sce0.1))%>%
  left_join(., data.west %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(typology) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()

val_preds.sce0.1 <- val_preds.sce0 %>% dplyr::select(mean_on, .pred)
names(val_preds.sce0.1)[2]<- ".pred.origin"
final_west <- plyr::join(final_preds.west, val_preds.sce0.1)
sum(final_west$.pred)
sum(final_west$.pred.origin)
ridership<- sce%>% drop_na()%>%dplyr::select(STOP_ID, mean_on)
ridership$mean_on <- exp(log(ridership$mean_on))
final_west2 <- plyr::join(final_west, ridership, type = "left")
names(final_west2)[1]<- ".pred.westcampus"

#Test the second scenario: adding building area at RiverSide
data.river <- data.river %>% drop_na()
data.river<- plyr::join(data.river, typology, type = "left")
#One hot encoding
data.river<- 
  as.data.table(data.river)%>%
  one_hot(cols = "SN_cat",
          dropCols = TRUE)

data.river <- 
  as.data.table(data.river)%>%
  one_hot(cols = "Crosstown_cat",
          dropCols = TRUE)

data.river <- 
  as.data.table(data.river)%>%
  one_hot(cols = "Express_cat",
          dropCols = TRUE)

data.river <- 
  as.data.table(data.river)%>%
  one_hot(cols = "Local_cat",
          dropCols = TRUE)
data.river <- 
  as.data.table(data.river)%>%
  one_hot(cols = "Flyer_cat",
          dropCols = TRUE)
data.river <- 
  as.data.table(data.river)%>%
  one_hot(cols = "NightOwl_cat",
          dropCols = TRUE)
data.river <- 
  as.data.table(data.river)%>%
  one_hot(cols = "HighFreq_cat",
          dropCols = TRUE)
data.river<- 
  as.data.table(data.river)%>%
  one_hot(cols = "InOut_cat",
          dropCols = TRUE)
data.river <- 
  as.data.table(data.river)%>%
  one_hot(cols = "Clockwise_cat",
          dropCols = TRUE)
data.river <- 
  as.data.table(data.river)%>%
  one_hot(cols = "utshuttle_cat",
          dropCols = TRUE)
data.river <- 
  as.data.table(data.river)%>%
  one_hot(cols = "Special_cat",
          dropCols = TRUE)
data.river <- data.river %>%dplyr::select(-STOP_ID)
data_split.river <- rsample::initial_split(data.river, strata = "mean_on", prop = 0.75)

model_rec.river <- recipe(mean_on ~ ., data = data.river) %>% #the "." means using every variable we have in the training dataset
  update_role(typology, new_role = "typology") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(typology, threshold = 0.005) %>%
  step_dummy(all_nominal(), -typology) %>%
  step_log(mean_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -mean_on) %>%
  step_scale(all_predictors(), -mean_on) #%>% #put on standard deviation scale
#step_ns(Latitude, Longitude, options = list(df = 4))
rf_wf.river <-
  workflow() %>% 
  add_recipe(model_rec.river) %>% 
  add_model(rf_plan)

rf_best_wf.river    <- finalize_workflow(rf_wf.river, rf_best_params.sce0)

rf_val_fit_geo.river<- rf_best_wf.river %>% 
  last_fit(split     = data_split.river,
           control   = control,
           metrics   = metric_set(rsq))

rf_val_pred_geo.river    <- collect_predictions(rf_val_fit_geo.river)

final_rf_val_pred_geo.river <- rf_val_pred_geo.river %>%
  dplyr::select(.pred,mean_on,.row)

final_preds.river <- rbind(data.frame(final_rf_val_pred_geo.river),data.frame(rf_best_OOF_preds.sce0.1))%>%
  left_join(., data.river %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(typology, .row), 
            by = ".row") %>% 
  dplyr::group_by(typology) %>%
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred))%>%
  ungroup()
final_preds.river2 <- final_preds.river %>% 
  dplyr::select(.pred, mean_on)
names(final_preds.river2)[1]<-".pred.Riverside"
final_preds.river2 <- plyr::join(final_west2, final_preds.river2, type = "left")

?join
sum(final_west2$.pred.origin)
sum(final_preds.river$.pred)
