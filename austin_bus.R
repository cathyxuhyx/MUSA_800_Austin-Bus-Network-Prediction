#load packages
library(tidyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(gridExtra)
# change the directory in order to load the data
agg <- read.csv('D:/Spring20/Practicum/data/MUSA Data - Stop Ridership Aggregated.csv')
disagg <- read.csv('Data/MUSA Disagregated Data Sample 01-06-2020 to 01-10-2020.csv')
austin <- st_read('https://data.austintexas.gov/api/geospatial/3pzb-6mbr?method=export&format=GeoJSON')
serviceArea <- st_read('D:/Spring20/Practicum/data/June2018/Service_Area.shp')
NewRoutes <- st_read('D:/Spring20/Practicum/data/NewRoutes.shp')
HighFreq <- st_read('D:/Spring20/Practicum/data/HighFrequency.shp')
Replaced <- st_read('D:/Spring20/Practicum/data/EliminatedReplacement.shp')
Eliminated <- st_read('D:/Spring20/Practicum/data/Eliminated.shp')

austin <- austin%>%
  st_transform(32140)

#turn dataframe into spacitial object
agg_sf <- agg%>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)%>%
  st_transform(32140)

#divide agg_sf data into before and after capremap
agg_before_sf <- agg_sf%>%
  filter(YEAR_ID == 2017 | (YEAR_ID == 2018 & MONTH_ID < 6))%>%
  group_by(STOP_ID)%>%
  summarise(avg_on = mean(AVERAGE_ON),
            avg_trips = mean(TRIPS))%>%
  mutate(ratio = avg_on / avg_trips)

agg_after_sf <- agg_sf%>%
  filter(YEAR_ID == 2019 | (YEAR_ID == 2018 & MONTH_ID > 5))%>%
  group_by(STOP_ID)%>%
  summarise(avg_on = mean(AVERAGE_ON),
            avg_trips = mean(TRIPS))%>%
  mutate(ratio = avg_on / avg_trips)


#scatterplot
Before_scatter <- ggplot()+
                  geom_point(data = agg_before_sf, aes(x=avg_trips, y=avg_on, color = STOP_ID))+
                  labs(title = "Ridership vs. Number of Trips",
                       subtitle = "Before CapRemap",
                       xlab = "Average Ridership",
                       ylab = "Average Trips")
After_scatter <-  ggplot()+
                  geom_point(data = agg_after_sf, aes(x=avg_trips, y=avg_on, color = STOP_ID))+
                  labs(title = "Ridership vs. Number of Trips",
                  subtitle = "After CapRemap",
                  xlab = "Average Ridership",
                  ylab = "Average Trips")
    
Scatterplots <- grid.arrange(Before_scatter,After_scatter,ncol = 2)

#mapping the stops by ridership

#doesn't show clear patterns when plotting all the stops
ggplot()+
  geom_sf(mapping = aes(), data = austin)+
  geom_sf(mapping = aes(color = ratio), data = agg_before_sf)+
  scale_colour_gradient(low = "light blue", high = "darkblue")


#below 25% as low ridership whereas higher than 75% as high ridership
quantile(agg_before_sf$ratio)
quantile(agg_after_sf$ratio)


Low_before <- ggplot(data=austin)+
                geom_sf(fill = "light grey")+
                geom_sf(data = subset(agg_before_sf, ratio<0.06505824), color = "darkblue")+
                labs(title = "Austin Bus Stops Ridership",
                subtitle = "Low ridership stops before CapRemap")
Low_after <- ggplot(data=austin)+
             geom_sf(fill = "light grey")+
              geom_sf(data = subset(agg_after_sf, ratio<0.03767514), color = "darkblue")+
              labs(title = "Austin Bus Stops Ridership",
              subtitle = "Low ridership stops after CapRemap")

Low_Compare <- grid.arrange(Low_before, Low_after, ncol = 2)


High_before <- ggplot(data=austin)+
  geom_sf(fill = "light grey")+
  geom_sf(data = subset(agg_before_sf, ratio>0.43755896), color = "darkblue")+
  labs(title = "Austin Bus Stops Ridership",
       subtitle = "Low ridership stops before CapRemap")

High_after <- ggplot(data=austin)+
  geom_sf(fill = "light grey")+
  geom_sf(data = subset(agg_after_sf, ratio>0.41095547), color = "darkblue")+
  labs(title = "Austin Bus Stops Ridership",
       subtitle = "Low ridership stops after CapRemap")

High_Compare <- grid.arrange(High_before, High_after, ncol = 2)

Zero_before <- ggplot(data=austin)+
  geom_sf(fill = "light grey")+
  geom_sf(data = subset(agg_before_sf, ratio==0), color = "darkblue")+
  labs(title = "Austin Bus Stops Ridership",
       subtitle = "Stops without Any Ridership before CapRemap")

Zero_after <- ggplot(data=austin)+
  geom_sf(fill = "light grey")+
  geom_sf(data = subset(agg_after_sf, ratio==0), color = "darkblue")+
  labs(title = "Austin Bus Stops Ridership",
       subtitle = "Stops without Any Ridership before CapRemap")
  
Zero_Compare <- grid.arrange(Zero_before, Zero_after, ncol = 2)

#
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdiction", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = Eliminated, aes(color = "Eliminated Routes"), lwd = 0.3, show.legend = "line")+
  geom_sf(data = Replaced, aes(color = "Eliminated but Replaced Routes"),lwd = 0.3, show.legend = "line")+
  geom_sf(data = HighFreq, aes(color = "High Frequency Routes"), lwd = 0.8, show.legend = "line")+
  geom_sf(data = NewRoutes, aes(color = "New Routes"),lwd = 0.8, show.legend = "line")+
  scale_colour_manual(values = c("Eliminated Routes" = "darkorange", "Eliminated but Replaced Routes" = "gold", "High Frequency Routes" = "dodgerblue", "New Routes" = "deeppink"),
                      guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid", "solid", "solid"), shape = c(NA, NA, NA, NA))))+
  labs(title = "Cap Remap Route Changes",
       subtitle = "City of Austin, June 2018")

