#load packages
library(tidyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(gridExtra)
library(RColorBrewer)
# change the directory in order to load the data
agg <- read.csv('D:/Spring20/Practicum/data/MUSA Data - Stop Ridership Aggregated.csv')
<<<<<<< HEAD
disagg <- read.csv('Data/MUSA Disagregated Data Sample 01-06-2020 to 01-10-2020.csv')
austin <- st_read('https://data.austintexas.gov/api/geospatial/3pzb-6mbr?method=export&format=GeoJSON')
serviceArea <- st_read('D:/Spring20/Practicum/data/June2018/Service_Area.shp')
NewRoutes <- st_read('D:/Spring20/Practicum/data/NewRoutes.shp')
HighFreq <- st_read('D:/Spring20/Practicum/data/HighFrequency.shp')
Replaced <- st_read('D:/Spring20/Practicum/data/EliminatedReplacement.shp')
Eliminated <- st_read('D:/Spring20/Practicum/data/Eliminated.shp')
=======
#disagg <- read.csv('Data/MUSA Disagregated Data Sample 01-06-2020 to 01-10-2020.csv')
austin <- st_read('https://data.austintexas.gov/api/geospatial/3pzb-6mbr?method=export&format=GeoJSON')
counties <- st_read("https://opendata.arcgis.com/datasets/0c2b3b7224ea444d879f36bbabb4df57_0.geojson")
cities <- st_read("https://opendata.arcgis.com/datasets/eed7edb94c364a30a8100f69e7c6a309_0.geojson")
schoolDist <- st_read("https://opendata.arcgis.com/datasets/21bfd7ce11024bceac32c8d4ba3a1da0_0.geojson")
NewRoutes <- st_read("D:/Spring20/Practicum/data/NewRoutes.shp")
HighFreq <- st_read("D:/Spring20/Practicum/data/HighFrequency.shp")
routes1801 <- st_read("D:/Spring20/Practicum/data/Jan2018/Routes.shp")
replacd <- st_read("D:/Spring20/Practicum/data/EliminatedReplacement.shp")
eliminated <- st_read("D:/Spring20/Practicum/data/Eliminated.shp")

#new scale function
new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}
>>>>>>> 09ae8f5dd705c3e64deaedcb5473cba9d99dd5ca

austin <- austin%>%
  st_transform(32140)

<<<<<<< HEAD
=======
counties <- counties%>%
  st_transform(32614)

cities <- cities%>%
  st_transform(32614)

schoolDist <- schoolDist%>%
  st_transform(32614)%>%
  st_contains(routes)%>%
  st_geometry()

schoolDist <- st_contains(routes)


  
counties <- subset(counties, COUNTY == "WILLIAMSON" | COUNTY == "TRAVIS")
cities <- subset(cities, MUNI_NM == "AUSTIN" | MUNI_NM == "JONESTOWN"|MUNI_NM == "LAGO VISTA"|MUNI_NM =="LEANDER"|MUNI_NM =="MANOR"|
                   MUNI_NM == "POINT VENTURE"|MUNI_NM =="SAN LEANNA"|MUNI_NM =="ROUND ROCK"|MUNI_NM =="VOLENTE")

>>>>>>> 09ae8f5dd705c3e64deaedcb5473cba9d99dd5ca
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

<<<<<<< HEAD
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

=======
#Capital Metro Service Area
serviceArea <- st_read("D:/Spring20/Practicum/data/Service_Area.shp")
routes <- st_read("D:/Spring20/Practicum/data/Routes.shp")

ggplot()+
  #geom_sf(data = cities, aes(color = MUNI_NM), size = 0.8)+
  #geom_sf(data = schoolDist, size = 0.8)+
  geom_sf(data = serviceArea, aes(fill = NAME), size = 0.8, alpha = 0.5)+
  #scale_color_discrete(name = "Jurisdictions")+
  scale_fill_manual(name = "Jurisdictions", values = rev(brewer.pal(9,"BuPu")))+
  geom_sf(data = routes1801, color = "grey90", alpha = 0.2)+
  labs(name = "Routes 2018.01", guide = "legend")+
  geom_sf(data = eliminated, color = "deeppink", size = 1)+
  labs(name = "Eliminated", guide = "legend")+
  labs(title = "Cap Remap Eliminated Routes")
  
>>>>>>> 09ae8f5dd705c3e64deaedcb5473cba9d99dd5ca
