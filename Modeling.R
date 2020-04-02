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
data <- all_2 %>%
  drop_na()

library(glmnet)
library(tidyr)

set.seed(717)

theme_set(theme_bw())

"%!in%" <- Negate("%in%")
g <- glimpse

#Slipt the data into training and testing sets
data_split <- rsample::initial_split(data, strata = "mean_on", prop = 0.75)

bus_train <- rsample::training(data_split)
bus_test  <- rsample::testing(data_split)
table(bus_train$label)
cv_splits_geo <- rsample::group_vfold_cv(bus_train,  strata = "mean_on", group = "label")
print(cv_splits_geo)

#Create recipe
model_rec <- recipe(mean_on ~ ., data = bus_train) %>% #the "." means using every variable we have in the training dataset
#  update_role(label, new_role = "label") %>% #This is more like to keep the neighborhood variable out of the model
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
  geom_path(color = "red") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title= "MAPE of each model on the testing set")
  theme_bw()
#MAE chart
ggplot(data = val_preds %>% 
           dplyr::select(model, MAE) %>% 
           distinct() , 
         aes(x = model, y = MAE, group = 1)) +
    geom_path(color = "red") +
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
palette4 <- c("#D2FBD4","#92BCAB","#527D82","#123F5A")


val_MAPE_by_hood %>%
  dplyr::select(label, model, MAE) %>%
  gather(Variable, MAE, -model, -label) %>%
  ggplot(aes(label, MAE)) + 
  geom_bar(aes(fill = model), position = "dodge", stat="identity") +
  scale_fill_manual(values = palette4) +
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
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}
palette5 <- c("#edf8fb","#b2e2e2","#66c2a4","#2ca25f","#006d2c")







#Map: MAPE of lm
MAE.nhood%>%
  filter(model=="lm") %>%
  ggplot() +
  #    geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = palette5,
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
  scale_fill_brewer(palette = palette5,
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
  scale_fill_brewer(palette = palette5,
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
  scale_fill_brewer(palette = palette5,
                    aesthetics = "fill",
                    labels=qBr(MAE.nhood,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of xgb in Neighborhoods") +
  mapTheme()

# plot 
ggplot(filter(val_MAPE_by_hood, model == "glmnet") %>% 
         mutate(label = fct_reorder(label, MAPE)),
       aes(x = label, y = MAPE)) +
  geom_bar(stat = "identity") +
#  scale_y_continuous(breaks = seq(0,75,5)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -45, hjust = 0)
  )
