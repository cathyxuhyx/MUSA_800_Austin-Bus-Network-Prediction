#load packages
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(sf)
library(gridExtra)
library(RColorBrewer)
library(osmdata)
library(tidycensus)
library(areal)
library(viridis)


# change the directory in order to load the data
agg <- read.csv('D:/Spring20/Practicum/data/MUSA Data - Stop Ridership Aggregated.csv')
Routes1801 <- st_read("D:/Spring20/Practicum/data/Jan2018/Routes.shp")
disagg <- read.csv('D:/Spring20/Practicum/data/MUSA Disagregated Data Sample 01-06-2020 to 01-10-2020.csv')
austin <- st_read('https://data.austintexas.gov/api/geospatial/3pzb-6mbr?method=export&format=GeoJSON')
serviceArea <- st_read('D:/Spring20/Practicum/data/June2018/Service_Area.shp')
NewRoutes <- st_read('D:/Spring20/Practicum/data/NewRoutes.shp')
HighFreq <- st_read('D:/Spring20/Practicum/data/HighFrequency.shp')
Replaced <- st_read('D:/Spring20/Practicum/data/EliminatedReplacement.shp')
Eliminated <- st_read('D:/Spring20/Practicum/data/Eliminated.shp')
Routes2001 <- st_read('D:/Spring20/Practicum/data/Routes.shp')
stops <- st_read('D:/Spring20/Practicum/data/Stops.shp')

Routes1801 <- Routes1801%>%
  mutate(capremap = "Before Cap Remap")

Routes2001 <- Routes2001%>%
  mutate(capremap = "After Cap Remap")

#new scale function
new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}

stops <- stops%>%
  st_transform(2278)

austin <- austin%>%
  st_transform(2278)


counties <- counties%>%
  st_transform(2278)

cities <- cities%>%
  st_transform(2278)

schoolDist <- schoolDist%>%
  st_transform(2278)%>%
  st_contains(routes)%>%
  st_geometry()


counties <- subset(counties, COUNTY == "WILLIAMSON" | COUNTY == "TRAVIS")
cities <- subset(cities, MUNI_NM == "AUSTIN" | MUNI_NM == "JONESTOWN"|MUNI_NM == "LAGO VISTA"|MUNI_NM =="LEANDER"|MUNI_NM =="MANOR"|
                   MUNI_NM == "POINT VENTURE"|MUNI_NM =="SAN LEANNA"|MUNI_NM =="ROUND ROCK"|MUNI_NM =="VOLENTE")


#turn dataframe into spacitial object
agg_sf <- agg%>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)%>%
  st_transform(2278)

stops_sf <- Stops%>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)%>%
  st_transform(2278)

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

ggplot()+
  geom_sf(data = Stops, aes(colour = STOP_ID))+
  geom_sf(data = Eliminated)
#overview map
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = Eliminated, aes(color = "Eliminated Routes"), lwd = 0.3, show.legend = "line")+
  geom_sf(data = Replaced, aes(color = "Eliminated but Replaced Routes"),lwd = 0.3, show.legend = "line")+
  geom_sf(data = HighFreq, aes(color = "High Frequency Routes"), lwd = 0.8, show.legend = "line")+
  geom_sf(data = NewRoutes, aes(color = "New Routes"),lwd = 0.8, show.legend = "line")+
  scale_colour_manual(values = c("Eliminated Routes" = "darkorange", "Eliminated but Replaced Routes" = "gold", "High Frequency Routes" = "dodgerblue", "New Routes" = "deeppink"),
                      guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid", "solid", "solid"), shape = c(NA, NA, NA, NA))))+
  labs(title = "Cap Remap Route Changes",
       subtitle = "City of Austin, June 2018")


#types of routes
#local
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = subset(Routes1801, ROUTETYPE == "Local"), color = "lightblue2",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Local"), color = "lightblue2",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "lightblue2", "After Cap Remap" = "lightblue2"),
                      #guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  facet_grid(~capremap)+
  labs(title = "Local Routes Before and After Cap Remap")

#HighFrequency

ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = subset(Routes1801, ROUTETYPE == "High Frequency"), color = "dodgerblue",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "High Frequency"), color = "dodgerblue",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "dodgerblue", "After Cap Remap" = "dodgerblue"),
                      #guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  facet_grid(~capremap)+
  labs(title = "High Frequency Routes Before and After Cap Remap")

#major changes grid arrange
grid.arrange(local, highFrequency, ncol = 1)

#Crosstown

ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = subset(Routes1801, ROUTETYPE == "Crosstown"), color = "greenyellow",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Crosstown"), color = "greenyellow",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "greenyellow", "After Cap Remap" = "greenyellow"),
                      #guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  facet_grid(~capremap)+
  labs(title = "Crosstown Routes Before and After Cap Remap")

#Feeder
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = subset(Routes1801, ROUTETYPE == "Feeder"), color = "lightcoral",lwd = 0.5, show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Feeder"), color = "lightcoral",lwd = 0.5, show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "lightcoral", "After Cap Remap" = "lightcoral"))+
                      #guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  facet_grid(~capremap)+
  labs(title = "Feeder Routes Before and After Cap Remap")


#Flyer
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = subset(Routes1801, ROUTETYPE == "Flyer"), color = "magenta2",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Flyer"), color = "magenta2",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "magenta2", "After Cap Remap" = "magenta2"),
                     # guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  facet_grid(~capremap)+
  labs(title = "Flyer Routes Before and After Cap Remap")

#Express
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = subset(Routes1801, ROUTETYPE == "Express"), color = "red",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Express"), color = "red",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "red", "After Cap Remap" = "red"),
                      #guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  facet_grid(~capremap)+
  labs(title = "Express Routes Before and After Cap Remap")

#minor changes grid arrange

grid.arrange(crosstown, feeder, flyer, express, special,ncol =2)


#UT Shuttle
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = subset(Routes1801, ROUTETYPE == "UT Shuttle"), color = "orange",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "UT Shuttle"), color = "orange",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "orange", "After Cap Remap" = "orange"),
                      #guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  facet_grid(~capremap)+
  labs(title = "UT Shuttle Before and After Cap Remap")

#Special
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = subset(Routes1801, ROUTETYPE == "Special"), color = "seashell2",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Special"), color = "seashell2",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "seashell2", "After Cap Remap" = "seashell2"),
   #                   guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  facet_grid(~capremap)+
  labs(title = "Speical Routes Before and After Cap Remap")

#Nigh Owl
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data = subset(Routes1801, ROUTETYPE == "Night Owl"), color = "slategray2",lwd = 0.5,show.legend = FALSE)+
  geom_sf(data = subset(Routes2001, ROUTETYPE == "Night Owl"), color = "slategray2",lwd = 0.5,show.legend = FALSE)+
  #scale_colour_manual(values = c("Before Cap Remap" = "slategray2", "After Cap Remap" = "slategray2"),
   #                   guide = guide_legend("Routes", override.aes = list(linetype = c("solid", "solid"))))+
  facet_grid(~capremap)+
  labs(title = "Nigh Owl Routes Before and After Cap Remap")

#no change grid arrange
grid.arrange(UTShuttle, special, nightowl, ncol = 1)

#####OSM#####
getOSM <- function(key,value){
  feature <- opq(bbox = 'Austin, Texas')%>%
    add_osm_feature(key = key, value = value) %>%
    osmdata_sf ()
  if(is.null(feature$osm_points)){
    feature_poly <- feature$osm_polygons%>%
      select(osm_id,geometry)%>%
      st_as_sf(coords = geometry, crs = 4326, agr = "constant")%>%
      st_transform(2278)
    return(feature_poly)
  } else {
  feature_pt <- feature$osm_points%>%
    select(osm_id,geometry)%>%
    st_as_sf(coords = geometry, crs = 4326, agr = "constant")%>%
    st_transform(2278)
  return (feature_pt)
  }
}

#shop
shop <- opq(bbox = 'Austin, Texas')%>%
  add_osm_feature(key = 'shop') %>%
  osmdata_sf ()

shop_pt <- shop$osm_points

shop_pt <- shop_pt%>%
  select(osm_id,
         name,
         addr.city,
         geometry)%>%
  st_as_sf(coords = geometry, crs = 4326, agr = "constant")%>%
  st_transform(2278)

university <- opq(bbox = 'Austin, Texas')%>%
  add_osm_feature(key = 'amenity',value = 'university') %>%
  osmdata_sf ()

university <- university$osm_polygons%>%
  select(geometry)%>%
  st_as_sf(coords = geometry, crs = 4326, agr = "constant")%>%
  st_transform(2278)

shop_pt <- shop_pt%>%
  select(osm_id,
         name,
         addr.city,
         geometry)%>%
  st_as_sf(coords = geometry, crs = 4326, agr = "constant")%>%
  st_transform(2278)

#commercial
commercial <- getOSM('building', 'commercial')
#retail
retail <- getOSM('building', 'retail')
#supermarket
supermkt <- getOSM('building', 'supermarket')
#office
office <- getOSM('building', 'office')
#residential
residential <- getOSM('building','residential')
#bar
bar <- getOSM('amenity', 'bar')
#school
school <- getOSM('amenity', 'school')
#uni
university <- getOSM('amenity', 'university')
#parking
parking <- getOSM('amenity', 'parking')
#statium
stadium <- getOSM('building', 'stadium')
#trainstation
trainstation <- getOSM('building', 'train_station')

#plot OSM
plotOSM <- function(OSM)
ggplot()+
  geom_sf(data = serviceArea, aes(fill = "Service Areas"))+
  geom_sf(data = subset(serviceArea, NAME == "Austin"), aes(fill = "Austin"))+
  scale_fill_manual(values = c("Service Areas" = "gray25", "Austin" = "black"), name = NULL,
                    guide = guide_legend("Jurisdictions", override.aes = list(linetype = "blank", shape = NA))) +
  geom_sf(data=parking,
          inherit.aes =FALSE,
          colour="#238443",
          fill="#004529",
          alpha=.5,
          size=1,
          shape=21)+
  labs(x="",y="")

#####buffer#####
StopBuff <- stops%>%
  st_buffer(1320)

StopBuff2 <- stops%>%
  st_buffer(2640)

#####census#####
options(tigris_use_cache = TRUE)
v17 <- load_variables(2017, "acs5", cache = TRUE)

Hays <- get_acs(state = "48", county = "209", geography = "tract", 
                     variables = "B01001_001", geometry = TRUE)
Travis <- get_acs(state = "48", county = "453", geography = "tract", 
                variables = "B01001_001", geometry = TRUE)
Williamson <- get_acs(state = "48", county = "491", geography = "tract", 
                variables = "B01001_001", geometry = TRUE)
######spatial join#####
bufferInit <- function(Buffer, Points, Name){
  if(class(Points$geometry) == "sfc_POINT"){
  Init <- st_join(Buffer%>% select(STOP_ID), Points, join = st_contains)%>%
  group_by(STOP_ID)%>%
    summarize(count = n())%>%
    rename(!!Name := count)
  }else {
    Init <- st_join(Buffer%>% select(STOP_ID), Points, join = st_intersects)%>%
      group_by(STOP_ID)%>%
      summarize(count = n())%>%
      rename(!!Name := count)
  }
}

CommercialInit <- bufferInit(StopBuff, commercial, 'commercial_count')
RetailInit <- bufferInit(StopBuff, retail, 'retail_count')
OfficeInit <- bufferInit(StopBuff, office, 'office_count')
ResidentialInit <- bufferInit(StopBuff, residential, 'residential_count')
SupermktInit <- bufferInit(StopBuff, supermkt, 'supermkt_count')
BarInit <- bufferInit(StopBuff, bar, 'bar_count')
UniInit <- bufferInit(StopBuff, university, 'university_count')
ParkingInit <- bufferInit(StopBuff, parking, 'parking_count')

#####buffer deomographics#####
#demo data bind
Population <- rbind(Travis, Williamson)%>%
  st_transform(2278)

Population_buff <- aw_interpolate(StopBuff, tid = STOP_ID, source = Population, sid = GEOID, weight = "sum",
                                  output = "sf", extensive = "estimate")
Population_buff$estimate <- round(Population_buff$estimate)

