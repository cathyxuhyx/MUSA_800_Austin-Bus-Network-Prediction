streets <- st_read('https://data.austintexas.gov/api/geospatial/m5w3-uea6?method=export&format=GeoJSON') %>% 
  st_transform(2278)

rivers <- st_read('https://data.austintexas.gov/api/geospatial/p2uq-mkbt?method=export&format=GeoJSON') %>% 
  st_transform(2278)

stops_js <- read.csv("D:/Spring20/Practicum/MUSA801-Web-App/data/js_test1.csv")
stop_sel <- c("X", "Y", "STOP_ID")
stops_js <- stops_js[stop_sel]

#####Frequency scenarios#####
complete_pred <- complete_route%>%
  dplyr::select(-pred_rf,
         -pred_xgb)
complete_pred$rf <- exp(predict(full_fit_model_rf0, complete_pred))
complete_pred$xgb <- unlist(exp(predict(full_fit_model_xgb0, complete_pred)))
####scenario 1
s1_wc1 <- c(599, 5864, 5528, 3512, 5024, 497, 5865, 1042)
s1_wc2 <- c(4122, 4120, 4099, 498, 2780, 5178)
s1_wc3 <- c(2003, 5634, 5353, 494, 5373, 5863, 495, 4122, 4123, 4121, 4979, 4120, 4153)


sce1 <- complete_route

# 
sce1[sce1$STOP_ID %in% s1_wc1,]$population <- sce1[sce1$STOP_ID %in% s1_wc1,]$population * 0.75
sce1[sce1$STOP_ID %in% s1_wc2,]$population <- sce1[sce1$STOP_ID %in% s1_wc2,]$population * 0.75
sce1[sce1$STOP_ID %in% s1_wc3,]$population <- sce1[sce1$STOP_ID %in% s1_wc3,]$population * 0.75

sce1$population <- as.numeric(sce1$population)
#get prediction
sce1$pred_rf <- exp(predict(full_fit_model_rf0, sce1))

####scenario 2
s2_ct <- c(5604,6363,2874,6306,1410,1409,1408,5505,4711,2870,4592,2392,4609,2390,2389,2388,5455,
           5932,6343,6301,6298,6296,4743,4742,1386,1385,1384,1383,1382,1381,1379,4534,5465,1377,
           5730,5150,1373,1372,1369,1368,1366,1365,1364,4284,5779,1028,1027,4296,4290,1475,1474,
           1467,1466,1465,2266,4040,4044,2263,2262,2261,773,772,771,2374,6357,4216,4234,6374)

sce2 <- complete_route

#
sce2[sce2$STOP_ID %in% s2_ct,]$count <- sce2[sce2$STOP_ID %in% s2_ct,]$count * 2

#get prediction
route <- st_read("D:/Spring20/Practicum/data/Routes.shp")%>%
  st_transform(2278)
sce2$pred_rf <- exp(predict(full_fit_model_rf0, sce2))
sce2_diff <- left_join(sce2,complete_pred, by = "STOP_ID")

sce2_diff <- sce2_diff%>%
  dplyr::select(STOP_ID, pred_rf,pred)%>%
  mutate(diff = as.numeric(unlist(pred_rf)) - unlist(pred))

sce2_diff_sf <- left_join(sce2_diff, stops, by = "STOP_ID")

sce2_diff_sf <- sce2_diff_sf%>%
  st_as_sf()

sce2_sf <- left_join(sce2, stops, by = "STOP_ID")

ggplot()+
  geom_sf(data = sce2_diff_sf, aes(color = diff))+
  scale_color_gradient2(low = "red",
                        high = "blue")+
  geom_sf(data = subset(route, ROUTE_ID == 300), alpha = 0.3)

sce2_sf <- sce2_sf%>%
  st_as_sf()
ggplot()+
  geom_sf(data = sce2_sf, aes(color = diff))+
  scale_color_gradient2(low = "red",
                        high = "blue")+
  geom_sf(data = subset(route, ROUTE_ID == 300), alpha = 0.3)

####scenario 3
s3 <- c(5304,5270,4548,4543,2821,6348,5859,5860,5862,612,610,606,603,5864,5865,4657,591,2606,5868,
        2767,4026,4039,5869,5552,6355,559,4382,6362,5873,5919,5724,4757,4572,4570,829,826,822,817,1058,
        4830,5351,603,5864,5865,4657,2606,5868,5498,781,777,2260,5875,6357,2370,6373)

sce3 <- complete_route

#
sce3[sce3$STOP_ID %in% s3,]$count <- sce3[sce3$STOP_ID %in% s3,]$count * 2

sce3$pred_rf <- exp(predict(full_fit_model_rf0, sce3))

sce3_diff <- left_join(sce3,complete_pred, by = "STOP_ID")

sce3_diff <- sce3_diff%>%
  dplyr::select(STOP_ID, pred_rf,pred)%>%
  mutate(diff = as.numeric(unlist(pred_rf)) - unlist(pred))

sce3_diff_sf <- left_join(sce3_diff, stops, by = "STOP_ID")

sce3_diff_sf <- sce3_diff_sf%>%
  st_as_sf()

sce3_sf <- left_join(sce3, stops, by = "STOP_ID")

ggplot()+
  geom_sf(data = sce3_diff_sf, aes(color = diff))+
  scale_color_gradient2(low = "red",
                        high = "blue")+
  geom_sf(data = subset(route, ROUTE_ID == 801 | ROUTE_ID == 803), color = "lightgrey")

####scenario 4
s4 <- c(2388, 5613, 5455, 5683, 5932, 5794, 5483, 6335, 5926, 5925, 6343, 6303, 6301, 1631, 6300, 1591, 
        1590, 1632, 6084, 1589, 1633, 1588, 4524, 6091, 6291, 6289, 1636, 5617, 4193, 5616, 3293, 6288, 
        5436, 6290, 5796, 5669, 5670, 5918, 3291, 3271, 6293, 6292)

sce4 <- complete_route

#
sce4[sce4$STOP_ID %in% s4,]$population <- sce4[sce3$STOP_ID %in% s4,]$population * 3

sce4$pred_rf <- exp(predict(full_fit_model_rf0, sce4))


####Riverside scenario
s5_r1 <- c(5779, 4288, 1005, 4286, 3526, 5774, 1012, 1022, 1021, 4299, 4300, 1013, 1020)
s5_r2 <- c(1364, 1353, 5065, 4283, 4284, 4285, 4302, 4301, 1014, 1018)

# make a copy
sce5 <- complete_route

# part 1 increase 469950 sqft
sce5[sce5$STOP_ID %in% s5_r1,]$building_area <- sce5[sce5$STOP_ID %in% s5_r1,]$building_area * 3

# part 2 increase 234975 sqft
sce5[sce5$STOP_ID %in% s5_r2,]$building_area <- sce5[sce5$STOP_ID %in% s5_r2,]$building_area * 3

sce5$pred_rf <- exp(predict(full_fit_model_rf0, sce5))


####feeder scenario
Palette5.1 <- c("#ca0020", "#f4a582","#e0e0e0","#92c5de", "#0571b0")
                
s6 <- c(2830,4923,2827,4927,4928,4926,2829,2825,4925,4924,4600,2831,2832,5220,5698,5881,5805,5854,
        5228,5778,4908,4909,5999,5380,5623,5310,5314,5313,550,551,553,5377,831,953,832,5736,3255,
        5466,4306,6432,6413,1363,4288,6428,4298,3527,1008,2273,2274,2275,4313,2276,5836,5882,6434,
        6085,5844,1619,1603,1604,3655,1606,1607,1608,4801,4613,6003,6004,6005,6006,6070,6183,5981,
        5007,5643,5914,4937,4938,2410,2416,4614,5982,6384,5983,5984,5607,5305,5278,4813,4815,4817,
        5342,5761,5765,5767,5768,5771,5301,2280,3311,5191,3315,3316,3317,3318,5592,5593,5045,5797,
        3323,3324,3238,5980,5986,5989,5927)

# make a copy
sce6 <- complete_route

# part 1 increase 469950 sqft
sce6[sce6$STOP_ID %in% s6,]$count <- sce6[sce6$STOP_ID %in% s6,]$count * 2


#sce6$pred_rf <- unlist(exp(predict(full_fit_model_rf0, sce6)))
sce6$pred_xgb <- unlist(exp(predict(full_fit_model_xgb0,sce6)))
sce6_diff <- left_join(sce6,complete_pred, by = "STOP_ID")

sce6_diff <- sce6_diff%>%
  dplyr::select(STOP_ID, pred_xgb,xgb)%>%
  mutate(Differences = unlist(pred_xgb) - unlist(xgb))

sce6_csv <- left_join(complete_route, sce6_diff, by = "STOP_ID")
sce6_csv <- left_join(sce6_csv, stops_js, by = "STOP_ID")

write.csv(sce6_csv, "D:/Spring20/Practicum/MUSA801-Web-App/data/FQ.csv")

sce6_diff_sf <- left_join(sce6_diff, stops, by = "STOP_ID")

sce6_diff_sf <- sce6_diff_sf%>%
  st_as_sf()

#bbox_new <- st_bbox(sce6_diff_sf)
ggplot() +
  geom_sf(data = subset(sce6_diff_sf, Differences == 0), aes(color = Differences), size =0.5)+
  geom_sf(data = rivers, color = "grey90", fill = "grey90")+
  geom_sf(data = streets %>% filter(road_class %in% c(0,1,2)), color = "grey90")+
  #geom_sf(data = st_centroid(na.omit(sce0_)), aes(color = mean_on), size = 1) +
  geom_sf(data = subset(route, ROUTE_ID == 201 | ROUTE_ID == 214 | ROUTE_ID == 228 | ROUTE_ID == 233 |ROUTE_ID == 237 | ROUTE_ID == 243 | ROUTE_ID == 271), color = "#E7B800",alpha = 0.5, lwd = 0.5)+
  geom_sf(data = subset(sce6_diff_sf, Differences != 0), aes(color = Differences), size =2)+
  scale_color_gradient2(low = "#ca0020",
                        mid = "#e0e0e0",
                        high = "#0571b0")+
  labs(title = "Scenario 1 - All Feeder Routes become High Frequency")+
  coord_sf(xlim = c(2313727, 2415189), ylim = c(13964004, 14128914)) + 
  #coord_sf(xlim = c(2310627, 2390089), ylim = c(13980004, 14059914)) +
  mapTheme()

# population
sce7 <- complete_route

# part 1 increase 469950 sqft
sce7[sce7$STOP_ID %in% s6,]$population <- round(sce7[sce7$STOP_ID %in% s6,]$population* 1.25)


sce7$pred_rf <- unlist(exp(predict(full_fit_model_rf0, sce7)))
sce7$pred_xgb <- unlist(exp(predict(full_fit_model_xgb0, sce7)))

# bldg
sce8 <- complete_route

# part 1 increase 469950 sqft
sce8[sce8$STOP_ID %in% s6,]$building_area <- sce8[sce8$STOP_ID %in% s6,]$building_area + 400000 


sce8$pred_rf <- exp(predict(full_fit_model_rf0, sce8))
sce8$pred_xgb <- unlist(exp(predict(full_fit_model_xgb0,sce8)))

sce8_diff <- left_join(sce8,complete_pred, by = "STOP_ID")

sce8_diff <- sce8_diff%>%
  dplyr::select(STOP_ID, pred_xgb,xgb)%>%
  mutate(Differences = pred_xgb - xgb)

sce8_csv <- left_join(complete_route, sce8_diff, by = "STOP_ID")
sce8_csv <- left_join(sce8_csv, stops_js, by = "STOP_ID")

write.csv(sce8_csv, "D:/Spring20/Practicum/MUSA801-Web-App/data/BA.csv")

sce8_diff_sf <- left_join(sce8_diff, stops, by = "STOP_ID")

sce8_diff_sf <- sce8_diff_sf%>%
  st_as_sf()
ggplot() +
  geom_sf(data = subset(sce8_diff_sf, Differences == 0), aes(color = Differences), size =0.5)+
  geom_sf(data = rivers, color = "grey90", fill = "grey90")+
  geom_sf(data = streets %>% filter(road_class %in% c(0,1,2)), color = "grey90")+
  #geom_sf(data = st_centroid(na.omit(sce0_)), aes(color = mean_on), size = 1) +
  geom_sf(data = subset(route, ROUTE_ID == 201 | ROUTE_ID == 214 | ROUTE_ID == 228 | ROUTE_ID == 233 |ROUTE_ID == 237 | ROUTE_ID == 243 | ROUTE_ID == 271), color = "#E7B800",alpha = 0.5, lwd = 0.5)+
  geom_sf(data = subset(sce8_diff_sf, Differences != 0), aes(color = Differences), size =2)+
  scale_color_gradient2(low = "#ca0020",
                        mid = "#e0e0e0",
                        high = "#0571b0")+
  labs(title = "Scenario 2 - Increased Building Areas around Stops")+
  coord_sf(xlim = c(2313727, 2415189), ylim = c(13964004, 14128914)) + 
  mapTheme()


# population
sce9 <- complete_route

# part 1 increase 469950 sqft
sce9[sce9$STOP_ID %in% s6,]$commercial <- sce9[sce9$STOP_ID %in% s6,]$commercial + 0.1
sce9[sce9$STOP_ID %in% s6,]$residential <- sce9[sce9$STOP_ID %in% s6,]$residential - 0.1

sce9$pred_rf <- unlist(exp(predict(full_fit_model_rf0, sce9)))
sce9$pred_xgb <- unlist(exp(predict(full_fit_model_xgb0,sce9)))
sce9_diff <- left_join(sce9,complete_pred, by = "STOP_ID")

sce9_diff <- sce9_diff%>%
  dplyr::select(STOP_ID, pred_xgb,xgb)%>%
  mutate(Differences = pred_xgb - xgb)

sce9_csv <- left_join(complete_route, sce9_diff, by = "STOP_ID")
sce9_csv <- left_join(sce9_csv, stops_js, by = "STOP_ID")

write.csv(sce9_csv, "D:/Spring20/Practicum/MUSA801-Web-App/data/LU.csv")

sce9_diff_sf <- left_join(sce9_diff, stops, by = "STOP_ID")

sce9_diff_sf <- sce9_diff_sf%>%
  st_as_sf()

ggplot() +
  geom_sf(data = subset(sce9_diff_sf, Differences == 0), aes(color = Differences), size =0.5)+
  geom_sf(data = rivers, color = "grey90", fill = "grey90")+
  geom_sf(data = streets %>% filter(road_class %in% c(0,1,2)), color = "grey90")+
  #geom_sf(data = st_centroid(na.omit(sce0_)), aes(color = mean_on), size = 1) +
  geom_sf(data = subset(route, ROUTE_ID == 201 | ROUTE_ID == 214 | ROUTE_ID == 228 | ROUTE_ID == 233 |ROUTE_ID == 237 | ROUTE_ID == 243 | ROUTE_ID == 271), color = "#E7B800",alpha = 0.5, lwd = 0.5)+
  geom_sf(data = subset(sce9_diff_sf, Differences != 0), aes(color = Differences), size =2)+
  scale_color_gradientn(low = "#ca0020",
                        mid = "#e0e0e0",
                        high = "#0571b0")+
  labs(title = "Scenario 3 - Land Use Changes around Stops")+
  coord_sf(xlim = c(2313727, 2415189), ylim = c(13964004, 14128914)) + 
  mapTheme()


#ARRANGE
library(gridExtra)
grid.arrange(freq, bldg_area, lu, ncol = 3)


#####nhood cross validation#####
full_fit_model_rf0 <- fit(rf_best_wf_sce0, data = complete_route)
full_fit_model_xgb0 <- fit(xgb_best_wf_sce0, data = complete_route)

complete_route$pred_rf <- as.numeric(unlist(exp(predict(full_fit_model_rf0, complete_route))))
complete_route$pred_xgb <- unlist(exp(predict(full_fit_model_xgb0, complete_route)))

# plot MAPE by model type

complete_route_nhood <- left_join(complete_route, nhood_sf, by = "label")%>%
  st_as_sf()

complete_route_pred <- complete_route_nhood%>%
  summarise(MAE_rf  = yardstick::mae_vec(mean_on, unlist(pred_rf)),
            MAE_xgb  = yardstick::mae_vec(mean_on, unlist(pred_xgb)),
            MAPE_rf = yardstick::mape_vec(mean_on, unlist(pred_rf)),
            MAPE_xgb = yardstick::mape_vec(mean_on, unlist(pred_xgb)))%>%
  st_set_geometry(NULL)
complete_route_pred%>%
  kable(caption = "MAE and MAPE Comparison between randomForest and XGBoost Models") %>%
  kable_styling("striped", full_width = F) 

pred_nhood_rf <- complete_route_nhood%>%
  mutate(Differences = unlist(pred_rf) - mean_on)%>%
  group_by(label)%>%
  summarise(RMSE = yardstick::rmse_vec(mean_on, unlist(pred_rf)),
            MAE  = yardstick::mae_vec(mean_on, unlist(pred_rf)),
            MAPE = yardstick::mape_vec(mean_on, unlist(pred_rf))) 

ggplot(data = complete_route_pred) %>% 
         dplyr::select(model, MAPE) %>% 
         distinct(), 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "blue") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  labs(title = "MAPE Comparisons")+
  theme_bw()


pred_nhood_rf%>%  
  ggplot() +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(pred_nhood_rf,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of rf in Neighborhoods") +
  mapTheme()  

pred_nhood_xgb <- complete_route_nhood%>%
  mutate(Differences = unlist(pred_xgb) - mean_on)%>%
  group_by(label)%>%
  summarise(RMSE = yardstick::rmse_vec(mean_on, unlist(pred_xgb)),
            MAE  = yardstick::mae_vec(mean_on, unlist(pred_xgb)),
            MAPE = yardstick::mape_vec(mean_on, unlist(pred_xgb))) 
pred_nhood_xgb%>%  
  ggplot() +
  geom_sf(aes(fill = q5(MAPE))) +
  scale_fill_brewer(palette = "Blues",
                    aesthetics = "fill",
                    labels=qBr(pred_nhood_xgb,"MAPE"),
                    name="Quintile\nBreaks, (%)") +
  labs(title="MAPE of xgb in Neighborhoods") +
  mapTheme()  

#####race and income####
library(tidycensus)
TravisPop <- 
  get_acs(geography = "tract", variables = c("B01001_001E","B01001A_001E"), 
          year = 2017, state=48, county=453, geometry=T) %>%
  st_transform(2278)  %>% 
  dplyr::select(variable, estimate, GEOID) %>%
  spread(variable, estimate) %>%
  rename(TotalPop = B01001_001,
         NumberWhites = B01001A_001) %>%
  mutate(percentWhite = NumberWhites / TotalPop,
         raceContext = ifelse(percentWhite > .5, "Majority_White", "Majority_Non_White")) 

WilliamPop <- 
  get_acs(geography = "tract", variables = c("B01001_001E","B01001A_001E"), 
          year = 2017, state=48, county=491, geometry=T) %>%
  st_transform(2278)  %>% 
  dplyr::select(variable, estimate, GEOID) %>%
  spread(variable, estimate) %>%
  rename(TotalPop = B01001_001,
         NumberWhites = B01001A_001) %>%
  mutate(percentWhite = NumberWhites / TotalPop,
         raceContext = ifelse(percentWhite > .5, "Majority_White", "Majority_Non_White")) 
Pop <- rbind(TravisPop, WilliamPop)%>%
  st_transform(2278)

ggplot() + 
  geom_sf(data = Pop, aes(fill = raceContext)) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Race Context", name="Race Context") +
  mapTheme() 

complete_stops <- left_join(complete_route, stops, by = "STOP_ID")%>%
  st_as_sf()

complete.Pop <- 
  st_join(st_centroid(complete_stops), Pop)%>%
  st_set_geometry(NULL) %>%
  mutate(diff_rf = pred_rf - mean_on,
         diff_xgb = pred_xgb - mean_on)%>%
  group_by(raceContext)%>%
  summarise(mean_diff_rf = mean(diff_rf),
            mean_diff_xgb = mean(diff_xgb),
            MPE_rf = mean(diff_rf/mean_on),
            MPE_xgb = mean(diff_xgb/mean_on))
  summarise(RMSE = yardstick::rmse_vec(mean_on, unlist(pred_xgb)),
            MAE  = yardstick::mae_vec(mean_on, unlist(pred_xgb)),
            MAPE = yardstick::mape_vec(mean_on, unlist(pred_xgb)))

complete.Pop%>%
  kable(caption = "Mean Error by neighborhood racial context") %>%
  kable_styling("striped", full_width = F) 

#poverty level
TravisPov <- 
  get_acs(geography = "tract", variables = c("B01001_001E","B06012_002E"), 
          year = 2017, state=48, county=453, geometry=T) %>%
  st_transform(2278)  %>% 
  dplyr::select(variable, estimate, GEOID) %>%
  spread(variable, estimate) %>%
  rename(TotalPop = B01001_001,
         NumberPoverty = B06012_002) %>%
  mutate(percentpov = NumberPoverty / TotalPop) 

#raceContext = ifelse(percentpov > .5, "Majority_White", "Majority_Non_White")

WilliamPov <- 
  get_acs(geography = "tract", variables = c("B01001_001E","B06012_002E"), 
          year = 2017, state=48, county=491, geometry=T) %>%
  st_transform(2278)  %>% 
  dplyr::select(variable, estimate, GEOID) %>%
  spread(variable, estimate) %>%
  rename(TotalPop = B01001_001,
         NumberPoverty = B06012_002) %>%
  mutate(percentpov = NumberPoverty / TotalPop) 
Pov <- rbind(TravisPov, WilliamPov)%>%
  st_transform(2278)%>%
  mutate(povContext = ifelse(percentpov > .126, "Majority_Poverty", "Majority_Non_Poverty"))

ggplot() + 
  geom_sf(data = Pov, aes(fill = povContext)) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Poverty Context", name="Poverty Context") +
  mapTheme() 

complete.Pov <- 
  st_join(st_centroid(complete_stops), Pov)%>%
  st_set_geometry(NULL)%>%
  mutate(diff_rf = pred_rf - mean_on,
         diff_xgb = pred_xgb - mean_on)%>%
  group_by(povContext)%>%
  summarise(mean_diff_rf = mean(diff_rf),
            mean_diff_xgb = mean(diff_xgb),
            #MPE_rf = mean(diff_rf/mean_on),
            #MPE_xgb = mean(diff_xgb/mean_on),
            MAE_rf  = yardstick::mae_vec(mean_on, unlist(pred_rf)),
            MAE_xgb  = yardstick::mae_vec(mean_on, unlist(pred_xgb)),
            MAPE_rf = yardstick::mape_vec(mean_on, unlist(pred_rf)),
            MAPE_xgb = yardstick::mape_vec(mean_on, unlist(pred_xgb)))

complete.Pov%>%
  kable(caption = "Mean Error, MAE & MAPE by neighborhood poverty context") %>%
  kable_styling("striped", full_width = F) 
 