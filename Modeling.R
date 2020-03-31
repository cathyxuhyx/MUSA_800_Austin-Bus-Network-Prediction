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

glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                           mixture = seq(0,1,0.25))

#Create workflow
lm_wf <-
  workflows::workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(lm_plan)

glmnet_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(glmnet_plan)

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

show_best(lm_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params     <- select_best(lm_tuned, metric = "rmse", maximize = FALSE)
glmnet_best_params <- select_best(glmnet_tuned, metric = "rmse", maximize = FALSE)
#Final workflow
lm_best_wf     <- finalize_workflow(lm_wf, lm_best_params)
glmnet_best_wf <- finalize_workflow(glmnet_wf, glmnet_best_params)

lm_val_fit_geo <- lm_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))
glmnet_val_fit_geo <- glmnet_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))


####################################Model Validation
# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds <- collect_predictions(lm_tuned) 
glmnet_best_OOF_preds <- collect_predictions(glmnet_tuned) %>% 
  filter(penalty  == glmnet_best_params$penalty[1] & mixture == glmnet_best_params$mixture[1])
# collect validation set predictions from last_fit model
lm_val_pred_geo     <- collect_predictions(lm_val_fit_geo)
glmnet_val_pred_geo <- collect_predictions(glmnet_val_fit_geo)

# Aggregate OOF predictions (they do not overlap with Validation prediction set)
OOF_preds <- rbind(data.frame(dplyr::select(lm_best_OOF_preds, .pred, mean_on), model = "lm"),
                   data.frame(dplyr::select(glmnet_best_OOF_preds, .pred, mean_on), model = "glmnet"))%>%
#                   data.frame(dplyr::select(rf_best_OOF_preds, .pred, Sale_Price), model = "RF"),
#                   data.frame(dplyr::select(xgb_best_OOF_preds, .pred, Sale_Price), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(.pred = exp(.pred),
         mean_on = exp(mean_on),
         RMSE = yardstick::rmse_vec(mean_on, .pred),
         MAE  = yardstick::mae_vec(mean_on, .pred),
         MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup()

#MAPE for lm and glmnet
ggplot(data = OOF_preds %>% 
         dplyr::select(model,MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  theme_bw()

# OOF predicted versus actual
ggplot(OOF_preds, aes(x =.pred, y = mean_on, group = model)) +
  geom_point(alpha = 0.3) +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  theme_bw()

# Aggregate predictions from Validation set
val_preds <- rbind(data.frame(lm_val_pred_geo, model = "lm"),
                   data.frame(glmnet_val_pred_geo, model = "glmnet"))%>%
#                   data.frame(rf_val_pred_geo, model = "rf"),
#                   data.frame(xgb_val_pred_geo, model = "xgb")) %>% 
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
#
ggplot(data = val_preds %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  theme_bw()

#Validation
ggplot(val_preds, aes(x =.pred, y = mean_on, group = model)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  theme_bw()

#Neighborhood validation
val_MAPE_by_hood <- val_preds %>% 
  group_by(label, model) %>% 
  summarise(RMSE = yardstick::rmse_vec(mean_on, .pred),
            MAE  = yardstick::mae_vec(mean_on, .pred),
            MAPE = yardstick::mape_vec(mean_on, .pred)) %>% 
  ungroup() 

# plot MAPE by Hood
ggplot(filter(val_MAPE_by_hood, model == "glmnet") %>% 
         mutate(label = fct_reorder(label, MAPE)),
       aes(x = label, y = MAPE)) +
  geom_bar(stat = "identity") +
#  scale_y_continuous(breaks = seq(0,75,5)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -45, hjust = 0)
  )
